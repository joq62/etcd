-define(SpecDir,"deployments").
-define(GitPathSpecs,"https://github.com/joq62/deployments.git").
-define(Extension,".deployment").

-define(TABLE,deployment).
-define(RECORD,deployment).

-record(?RECORD,{
		 name,
		 deployment_list
		}).
