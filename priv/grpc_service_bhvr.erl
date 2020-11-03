%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service {{unmodified_service_name}}.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on {{datetime}} and should not be modified manually

-module({{module_name}}_bhvr).

{{#methods}}
%% @doc {{^input_stream}}{{^output_stream}}Unary RPC{{/output_stream}}{{/input_stream}}
-callback {{method}}({{^input_stream}}{{^output_stream}}{{pb_module}}:{{input}}(), grpc:metadata(){{/output_stream}}{{/input_stream}})
    -> {{^input_stream}}{{^output_stream}}{ok, {{pb_module}}:{{output}}(), grpc:metadata()}{{/output_stream}}{{/input_stream}}
     | {error, grpc_cowboy_h:error_response()}.

{{/methods}}
