-module(sfdc_test).
-export([setup_initial_objects/3,login/0]).

-include_lib("eunit/include/eunit.hrl").

login()->
    application:start(inets),
    application:start(ssl),
    LoginInfo=sfdc:login("eerla@force.hccp.org", "password", "Xtsso4fWvK6uczst6SNtBEUK4"),
    LoginInfo.
    

xml_to_sobject_conversion_test()->
    UserInfoSoap="<?xml version=\"1.0\" encoding=\"UTF-8\"?><soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\"    xmlns=\"urn:partner.soap.sforce.com\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><soapenv:Body><getUserInfoResponse><result><accessibilityMode>false</accessibilityMode><currencySymbol>$</currencySymbol><orgDefaultCurrencyIsoCode>USD</orgDefaultCurrencyIsoCode><orgDisallowHtmlAttachments>false</orgDisallowHtmlAttachments><orgHasPersonAccounts>false</orgHasPersonAccounts><organizationId>00DA0000000aDfHMAU</organizationId><organizationMultiCurrency>false</organizationMultiCurrency><organizationName>HCCP</organizationName><profileId>00eA0000000tt1xIAA</profileId><roleId xsi:nil=\"true\"/><userDefaultCurrencyIsoCode xsi:nil=\"true\"/><userEmail>spam@hccp.org</userEmail><userFullName>Agner Erlang</userFullName><userId>005A0000001KH08IAG</userId><userLanguage>en_US</userLanguage><userLocale>en_US</userLocale><userName>eerla@force.hccp.org</userName><userTimeZone>America/Los_Angeles</userTimeZone><userType>Standard</userType><userUiSkin>Theme3</userUiSkin></result></getUserInfoResponse></soapenv:Body></soapenv:Envelope>",
    ExpectedUserInfo=get_user_info_sobject(),
    ExpectedUserInfo=sfdc:get_user_info_sobject_from_soap_response(UserInfoSoap).
    
add_note_functional_test()->
    CandidateId="a06A0000007fDIG",
    OwnerId="005A0000001KH08",
    Note=[ {"type", "string", "Note"},
	  {"ParentId", "string", CandidateId},
	  {"Title", "string", "My Test"},
	  {"Body", "string", "test content"},
	  {"OwnerId", "string", OwnerId}
	 ],
    [{sessionId,SessionId}, {serverUrl, Endpoint}]=login(),
    sfdc:create(Note, SessionId, Endpoint),
    sfdc:logout(SessionId, Endpoint).

delete_notes_functional_test()->
    [{sessionId,SessionId}, {serverUrl, Endpoint}]=login(),
    {IsDone, _, Size, Records}=sfdc:soql_query("select Id from Note where ParentId='a06A0000007fDIGIA2'", SessionId, Endpoint),
    F=fun(Record)->
	      [_,{"Id",_,Id},_]=Record,
	      [{sessionId,SessionId}, {serverUrl, Endpoint}]=login(),
	      sfdc:delete(Id, SessionId, Endpoint),	      
	      sfdc:logout(SessionId, Endpoint)
      end,
    Ids=lists:map(F, Records).
    
    
setup_initial_objects(Count, SessionId, Endpoint)->
    CandidateId="a06A0000007fDIG",
    OwnerId="005A0000001KH08",
    Note=[ {"type", "string", "Note"},
	  {"ParentId", "string", CandidateId},
	  {"Title", "string", lists:append("My Test", integer_to_list(Count))},
	  {"Body", "string", lists:append("test content", integer_to_list(Count))},
	  {"OwnerId", "string", OwnerId}
	 ],
    sfdc:create(Note, SessionId, Endpoint),
    case Count of
	1 -> ok;
	_ -> setup_initial_objects(Count-1, SessionId, Endpoint)
    end.
    
    


login_functional_test()->
     [{sessionId,SessionId}, {serverUrl, Endpoint}]=login().

happy_path_functional_test()->
    [{sessionId,SessionId}, {serverUrl, Endpoint}]=login(),
    ExpectedUserInfo=get_user_info_sobject(),
    ExpectedUserInfo=sfdc:get_user_info(SessionId, Endpoint),
    ExpectedQueryResults=sfdc:soql_query("select Id, FolderId, IsDeleted, Name, DeveloperName, NamespacePrefix, ContentType, Type, IsPublic, BodyLength, Url, Description, Keywords, IsInternalUseOnly, AuthorId, CreatedDate, CreatedById, LastModifiedDate, LastModifiedById, SystemModstamp, IsBodySearchable from Document", SessionId, Endpoint),
    validate_query(ExpectedQueryResults, "true", 1, 23),
    SecondExpectedQueryResults=sfdc:soql_query("select Id, Username, LastName, FirstName, Name, CompanyName, Division, Department, Title from User", SessionId, Endpoint),
    validate_query(SecondExpectedQueryResults, "true", 4, 11),
    ExpectedQueryAllResults=sfdc:soql_query_all("select Id, Username, LastName, FirstName, Name, CompanyName, Division, Department, Title from User", SessionId, Endpoint),
    validate_query(ExpectedQueryAllResults, "true", 4, 11),
    x=sfdc:get_server_timestamp(SessionId, Endpoint).


validate_query(Results, ExpectedIsDone, ExpectedSize, ExpectedNumberOfAttributesPerRecord)->
    {IsDone, _, Size, Records}=Results,
    ExpectedIsDone=IsDone,
    ExpectedSize=Size,
    [FirstRecord|_]=Records,
    ExpectedNumberOfAttributesPerRecord=length(FirstRecord).    


%erlang_object_query_functional_test()->
%    [{sessionId,SessionId}, {serverUrl, Endpoint}]=login(),
%    {IsDone, _, Size, Records}=sfdc:soql_query("select Id, OwnerId, MyDescription__c from ErlangTestObject__c", SessionId, Endpoint).
    

get_user_info_sobject()->
    [{"accessibilityMode","string","false"},
     {"currencySymbol","string","$"},
     {"orgDefaultCurrencyIsoCode","string","USD"},
     {"orgDisallowHtmlAttachments","string","false"},
     {"orgHasPersonAccounts","string","false"},
     {"organizationId","string","00DA0000000aDfHMAU"},
     {"organizationMultiCurrency","string","false"},
     {"organizationName","string","HCCP"},
     {"profileId","string","00eA0000000tt1xIAA"},
     {"roleId","string",[]},
     {"userDefaultCurrencyIsoCode","string",[]},
     {"userEmail","string","spam@hccp.org"},
     {"userFullName","string","Agner Erlang"},
     {"userId", "string", "005A0000001KH08IAG"},
     {"userLanguage", "string", "en_US"},
     {"userLocale", "string", "en_US"},
     {"userName", "string", "eerla@force.hccp.org"},
     {"userTimeZone", "string", "America/Los_Angeles"},
     {"userType", "string", "Standard"},
     {"userUiSkin", "string", "Theme3"}].


