-define(SpecDir,"cluster_specs").
-define(GitPathSpecs,"https://github.com/joq62/cluster_specs.git").

-define(TABLE,cluster_spec).
-define(RECORD,cluster_spec).

-record(?RECORD,{
		 spec_id,
		 deployment_spec,
		 cookie_str
		}).
