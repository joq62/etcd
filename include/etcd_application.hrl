-define(SpecDir,"application_specs").
-define(GitPathSpecs,"https://github.com/joq62/application_specs.git").
-define(Extension,".application").
-define(TABLE,application_spec).
-define(RECORD,?TABLE).
-record(?RECORD,{
		 appl_name,
		 vsn,
		 app,
		 erl_args,
		 git_path	
		}).


