-module(sfdc).
-export([login/3, login/4]).




%% get login string 

login (Username, Password, SecurityToken)->
    login(Username, Password, SecurityToken, "https://www.salesforce.com/services/Soap/c/18.0").

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
    lists:append([create_xml_declaration(), create_soap_envelope(create_soap_body(create_login_block(Username, Password)))]).

create_session_header(SessionId)->
    lists:append(["<SessionHeader xmlns=\"urn:partner.soap.sforce.com\"><sessionId>", SessionId, "</sessionId></SessionHeader>"]).



create_xml_declaration () ->
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>".


%% XML support

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

create_soap_envelope (SoapBody) ->
    lists:append(["<soap:Envelope xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">", SoapBody, "</soap:Envelope>"]). 

create_soap_header(Xml)->
    lists:append(["<soap:Header>",Xml,"</soap:Header>"]).
    

create_soap_body(Xml) ->
    lists:append(["<soap:Body>", Xml, "</soap:Body>"]).

create_login_block (Username, Password) ->
    lists:append(["<login xmlns=\"urn:enterprise.soap.sforce.com\"><username>", Username, "</username><password>", Password, "</password></login>"]).

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






