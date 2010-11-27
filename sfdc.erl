-module(sfdc).
-export([login/3, login/4, update/2, update/3, get_user_info/1, get_user_info/2, get_user_info_sobject_from_soap_response/1, soql_query/3, soql_query_all/3, soql_query_more/3, get_all_results_for_query/3]).




%% get login string 


get_default_endpoint()->
    "https://www.salesforce.com/services/Soap/u/18.0".



%% Login

login (Username, Password, SecurityToken)->
    login(Username, Password, SecurityToken, get_default_endpoint()).

login (Username, Password, SecurityToken, Endpoint)->
    LoginXml=create_login_request(Username, lists:append([Password, SecurityToken])),
     {ok, {{HttpVersion, ResponseCode, ResponseCodeDescription}, ResponseHeaders, ResponseBody}}=http:request(post, {Endpoint, [{"SOAPAction:", "\"\""}], "text/xml", LoginXml}, [],[]),
    case(ResponseCode) of
	200  -> io_lib:write_string(ResponseBody);
	500 -> io_lib:write_string(ResponseBody)
    end,
    SimplifiedXml=parse_xml(ResponseBody),
    case is_soap(SimplifiedXml) of 
	ok -> Body=get_body_from_envelope(SimplifiedXml),process_body_for_login(Body);
	_ -> {err, "Received response that was not not SOAP"}
    end.
 
	    
process_body_for_login (Body) ->	
    case is_fault(Body) of 
	ok -> {err, get_fault(Body)};
	_ -> extract_successful_login_info(Body)
    end.
	    
extract_successful_login_info(Body)->				   
    LoginResponse=get_body_content(Body),
    {_,_,LoginResponseChildren}=LoginResponse,
    [Result]=LoginResponseChildren,
    {_,_,ResultChildren}=Result,
    [_,_,_,ServerUrlElement,SessionIdElement|_]=ResultChildren,
    {_,_,[ServerUrl]}=ServerUrlElement,
    {_,_,[SessionId]}=SessionIdElement,
    [{sessionId,SessionId},{serverUrl,ServerUrl}].
    
    
create_login_request (Username, Password)->
    lists:append([create_xml_declaration(), create_soap_envelope([], create_soap_body(create_login_block(Username, Password)))]).

create_session_header(SessionId)->
    lists:append(["<SessionHeader xmlns=\"urn:partner.soap.sforce.com\"><sessionId>", SessionId, "</sessionId></SessionHeader>"]).


%% OPERATION: Update

update(SObject, SessionId)->
    update(SObject, SessionId, get_default_endpoint()).

update(SObject, SessionId, Endpoint)->
    SessionHeader=create_session_header(SessionId),
    UpdateBody=create_update(SObject),
    UpdateSoapMessage=create_soap_envelope(create_soap_header(SessionHeader), create_soap_body(UpdateBody)),
    send_soap_message(UpdateSoapMessage, Endpoint).

create_update(SObject)->
    lists:append(["<m:update xmlns:m=\"urn:partner.soap.sforce.com\" xmlns:sobj=\"urn:sobject.partner.soap.sforce.com\"><m:sObjects>", get_xml_for_sobject(SObject,[]), "</m:sObjects></m:update>"]).


% OPERATION: getUserInfo
get_user_info(SessionId)->
    get_user_info(SessionId, get_default_endpoint()).

get_user_info(SessionId, Endpoint)->
    SessionHeader=create_session_header(SessionId),
    GetUserInfoBody="<getUserInfo xmlns=\"urn:partner.soap.sforce.com\"/>",
    GetUserInfoSoapMessage=create_soap_envelope(create_soap_header(SessionHeader), create_soap_body(GetUserInfoBody)),
    SoapResponse=send_soap_message(GetUserInfoSoapMessage, Endpoint),
    get_user_info_sobject_from_soap_response(SoapResponse).

get_user_info_sobject_from_soap_response(SoapResponse)->
    Xml=parse_xml(SoapResponse),
    BodyXml=get_body_from_envelope(Xml),
    {getUserInfoResponse,_,[{result,_,UserInfoXml}]}=get_body_content(BodyXml),
    convert_xml_to_sobject(UserInfoXml,[]).



%OPERATION: Query

soql_query(QueryString, SessionId, Endpoint)->
    SessionHeader=create_session_header(SessionId),
    QueryBody=lists:append(["<query xmlns=\"urn:partner.soap.sforce.com\"><queryString>", QueryString, "</queryString></query>"]),
    QuerySoapMessage=create_soap_envelope(create_soap_header(SessionHeader), create_soap_body(QueryBody)),
    SoapResponse=send_soap_message(QuerySoapMessage, Endpoint),
    get_query_results_from_soap_response(SoapResponse).


soql_query_all(QueryString, SessionId, Endpoint)->
    SessionHeader=create_session_header(SessionId),
    QueryBody=lists:append(["<queryAll xmlns=\"urn:partner.soap.sforce.com\"><queryString>", QueryString, "</queryString></queryAll>"]),
    QuerySoapMessage=create_soap_envelope(create_soap_header(SessionHeader), create_soap_body(QueryBody)),
    SoapResponse=send_soap_message(QuerySoapMessage, Endpoint),
    get_query_results_from_soap_response(SoapResponse).

soql_query_more(QueryLocator, SessionId, Endpoint)->
    SessionHeader=create_session_header(SessionId),
    QueryBody=lists:append(["<queryMore xmlns=\"urn:partner.soap.sforce.com\"><queryLocator>", QueryLocator, "</queryLocator></queryMore>"]),
    QuerySoapMessage=create_soap_envelope(create_soap_header(SessionHeader), create_soap_body(QueryBody)),
    SoapResponse=send_soap_message(QuerySoapMessage, Endpoint),
    get_query_results_from_soap_response(SoapResponse).


get_all_results_for_query(QueryString, SessionId, Endpoint)->
    {IsDone, QueryLocator, _, Results}=soql_query(QueryString, SessionId, Endpoint),
    case IsDone of
	"true"->
	    Results;
	"false"->
	    lists:append(Results, get_more_results_for_query(QueryLocator, SessionId, Endpoint))
    end.

get_more_results_for_query(QueryLocator, SessionId, Endpoint)->
    {IsDone, QueryLocator, _, Results}=soql_query_more(QueryLocator, SessionId, Endpoint),
    case IsDone of
	"true"->
	    Results;
	"false"->
	    lists:append(Results, get_more_results_for_query(QueryLocator, SessionId, Endpoint))
    end.
									      

    

get_query_results_from_soap_response(SoapResponse)->
    Xml=parse_xml(SoapResponse),
    BodyXml=get_body_from_envelope(Xml),
    {_,_,[{result,_,QueryResponse}]}=get_body_content(BodyXml),
    [{done,_,[IsDone]}, {queryLocator,_,QueryLocator}|TheRest]=QueryResponse,
    {SizeInt, SobjectRecords}=get_query_results_from_record_set(TheRest),
    {IsDone, QueryLocator, SizeInt, SobjectRecords}.


get_query_results_from_record_set(RecordSet)->
    get_query_results_from_record_set(RecordSet,[]).

get_query_results_from_record_set([H|T],Results)->
    
    case H of 
	{records,_,_}->
	    {records,[{'xsi:type',_}],Records}=H,
	    RecordsSoFar=lists:append(Results, [convert_xml_to_sobject(Records, [])]),
	    get_query_results_from_record_set(T,RecordsSoFar);
	{size,_,_}->
	    {_,_,[Size]}=H,
	    {SizeInt,_}=string:to_integer(Size),
	    {SizeInt, Results}
    end;
get_query_results_from_record_set([],Results)->
    Results.
    




%SObject

get_xml_for_sobject([H|T],Xml)-> 
    {Name,Type,Value}=H,
    get_xml_for_sobject(T, lists:append([Xml, "<sobj:", Name, " xsi:type=\"", lists:append(["xsd:",Type]), "\">", Value, "</sobj:", Name, ">"]));
get_xml_for_sobject([],Xml) ->
    Xml.

convert_xml_to_sobject([H|T], Sobject)->
    {Name,_,Values}=H,
    MySobject=lists:append(Sobject,[{clean_prefix(atom_to_list(Name)), "string", get_value_from_sobject_xml(Values)}]),
    convert_xml_to_sobject(T, MySobject);
convert_xml_to_sobject([], Sobject)->    
   Sobject. 

clean_prefix(Name)->
    PrefixEnd=string:str(Name, ":"),
    case PrefixEnd of 
	0->Name;
	_-> string:substr(Name,1+PrefixEnd)
    end.



get_value_from_sobject_xml(XmlValue)->
    case length(XmlValue) of
	1 -> [Value]=XmlValue,Value;
	0 ->[];	     
	_ -> convert_xml_to_sobject(XmlValue,[])
    end.
	    

    

%% XML support

create_xml_declaration () ->
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>".


parse_xml(Message)->
    {Xml, _}=xmerl_scan:string(Message),
    xmerl_lib:simplify_element(Xml).
    
is_namespace_prefixed_element(SimplifiedXml, UnqualifiedName) ->			
    {QualifiedName, _, _}=SimplifiedXml,
    ReversedUnqualifiedName=lists:append([lists:reverse(UnqualifiedName), ":"]),
    Reversed=lists:reverse(atom_to_list(QualifiedName)),
    case (lists:prefix(ReversedUnqualifiedName, Reversed)) of
	true->ok;
	false->err
    end.



%% SOAP Support

create_soap_envelope (SoapHeader, SoapBody) ->
    lists:append(["<soap:Envelope xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\">", SoapHeader, SoapBody, "</soap:Envelope>"]). 

create_soap_header(Xml)->
    lists:append(["<soap:Header>",Xml,"</soap:Header>"]).
    
create_soap_body(Xml) ->
    lists:append(["<soap:Body>", Xml, "</soap:Body>"]).

create_login_block (Username, Password) ->
    lists:append(["<login xmlns=\"urn:partner.soap.sforce.com\"><username>", Username, "</username><password>", Password, "</password></login>"]).

get_body_from_envelope(EnvelopeXml)->
    {_, _, ChildElements}=EnvelopeXml,
    [BodyElement]=ChildElements,
    case (is_namespace_prefixed_element(BodyElement, "Body")) of
	 ok -> BodyElement; 
	 err -> err
	end.
		
get_fault(BodyXml)->   
    FaultElement=get_body_content(BodyXml),
    {_,_,FaultChildElements}=FaultElement,
    [FaultCode, FaultString, Detail]=FaultChildElements,
    {faultcode,_,[FaultCodeValue]}=FaultCode,
    {faultstring,_,[FaultMessage]}=FaultString,
    {detail,_,[FaultDetail]}=Detail,
    [{faultcode, FaultCodeValue},{faultstring, FaultMessage},{detail,FaultDetail}].

get_body_content(BodyXml)->
    {_,_,ChildElements}=BodyXml,
    [BodyContent]=ChildElements,
    BodyContent.

is_fault(BodyElement) ->
   {_, _, ChildElements}=BodyElement,
    [ChildElement]=ChildElements,
    is_namespace_prefixed_element(ChildElement,"Fault").

is_soap(SimplifiedXml) ->
   is_namespace_prefixed_element(SimplifiedXml,"Envelope").


send_soap_message(SoapMessage, Endpoint)->
    {ok, {{_, _, _}, _, ResponseBody}}=http:request(post, {Endpoint, [{"SOAPAction:", "\"\""}], "text/xml", SoapMessage}, [],[]),
    ResponseBody.




