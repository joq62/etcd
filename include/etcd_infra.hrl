-define(SpecDir,"infra_specs").
-define(GitPathSpecs,"https://github.com/joq62/infra_specs.git").
-define(Extension,".infra").

-define(TABLE,infra).
-define(RECORD,?TABLE).
-record(?RECORD,{
		 spec_id,
		 connect_nodes,
		 cookie_str,
		 num_workers
		}).
