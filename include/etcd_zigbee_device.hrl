-define(SpecDir,"zigbee_specs").
-define(GitPathSpecs,"https://github.com/joq62/zigbee_specs.git").
-define(Extension,".device").

-define(TABLE,zigbee_device).
-define(RECORD,?TABLE).
-record(?RECORD,{
		 name,
		 modelid,
		 device_type,
		 state,
		 type,
		 module
		}).
