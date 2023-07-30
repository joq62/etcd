-define(ProviderSpecDir,"provider_specs").
-define(GitPathProviderSpecs,"https://github.com/joq62/provider_specs.git").

-define(TABLE,provider_spec).
-define(RECORD,?TABLE).
-record(?RECORD,{
		 appl_name,
		 vsn,
		 app,
		 erl_args,
		 git_path	
		}).


