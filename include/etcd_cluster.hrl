-define(SpecDir,"cluster_specs").
-define(GitPathSpecs,"https://github.com/joq62/cluster_specs.git").
-define(Extension,".cluster").



-record(deployment_record,{
			   node_name,
			   node,
			   dir,
			   provider,
			   host
			  }).

	

-define(TABLE,cluster).
-define(RECORD,?TABLE).

%% 
-record(?RECORD,{
		 name,
		 cookie_str,        % "cookie"
		 deployment_spec,   %
		 deployment_records          %[{node_name,node,dir,provider,host,creation_time}
						% {node_name,node,node_dir}
		}).
