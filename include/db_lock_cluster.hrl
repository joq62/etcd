
-define(TABLE,cluster).
-define(RECORD,cluster).

-record(?RECORD,{
		 lock_id,
		 transaction_id,
		 time,
		 status
		}).
