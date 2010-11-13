-module(sfdc).
-export([login/2, login/3]).


%% XML support

parse_xml(Message)->
    {Xml, _}=xmerl_scan:string(Message),
    xmerl_lib:simplify_element(Xml).
    


%% login...

login (Username, Password)->
    login(Username, Password, "https://www.salesforce.com/services/Soap/c/18.0").

login (Username, Password, Endpoint)->
    LoginXml=create_login_request(Username, Password),
     {ok, {{HttpVersion, ResponseCode, ResponseCodeDescription}, ResponseHeaders, ResponseBody}}=http:request(post, {Endpoint, [{"SOAPAction:", "\"\""}], "text/xml", LoginXml}, [],[]),
    case(ResponseCode) of
	200  -> io_lib:write_string(ResponseBody);
	500 -> io_lib:write_string(ResponseBody)
    end,
    SimplifiedXml=parse_xml(ResponseBody),
    case is_soap(SimplifiedXml) of 
	ok ->  
	    
	
    
create_login_request (Username, Password)->
    lists:append([create_xml_declaration(), create_soap_envelope(create_soap_body(create_login_block(Username, Password)))]).


create_xml_declaration () ->
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>".



get_value_from_xml ([H|T], Name) ->
    
    case (H) of
	{Name, _, [Value|Rest]} ->
	     Value;
	_ -> get_value_from_xml(T, Name)
		 end;
get_value_from_xml ([_|_], Name) ->
    "".


%% SOAP Support

create_soap_envelope (SoapBody) ->
    lists:append(["<soap:Envelope xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">", SoapBody, "</soap:Envelope>"]). 

create_soap_body(Xml) ->
    lists:append(["<soap:Body>", Xml, "</soap:Body>"]).

create_login_block (Username, Password) ->
    lists:append(["<login xmlns=\"urn:enterprise.soap.sforce.com\"><username>", Username, "</username><password>", Password, "</password></login>"]).

get_body_from_envelope(EnvelopeXml)->
    {_, _, ChildElements}=EnvelopeXml,
    
is_soap(SimplifiedXml) ->
   {QualifiedName, NameSpaces, ChildElements}=SimplifiedXml,
    Reversed=lists:reverse(atom_to_list(QualifiedName)),
    case (Reversed) of
	"epolevnE:"++_->ok;
	_->err
    end.

is_namespace_prefixed_element(SimplifiedXml, UnqualifiedName) ->			
    {QualifiedName, _, _}=SimplifiedXml,
    ReversedUnqualifiedName=lists:append([lists:reverse(UnqualifiedName), ":"]),
    Reversed=lists:reverse(atom_to_list(QualifiedName)),
    case (Reversed) of
	ReversedUnqualifiedName++_->ok;
	_->err
    end.







%%create_element(Name, Nam)
