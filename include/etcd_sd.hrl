
%% -define(RefreshTime,60*1000).
-define(RefreshTime,2*1000).



-define(TABLE,sd).
-define(RECORD,sd).

-record(?RECORD,{
		 service_id,
		 module,
		 node,
		 time
		}).
