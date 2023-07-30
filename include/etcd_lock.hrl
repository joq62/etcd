
-define(TABLE,lock).
-define(RECORD,lock).

-record(?RECORD,{
		 lock,
		 transaction,
		 time,
		 status
		}).
