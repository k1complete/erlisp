-type kind() :: 'function' | 'callback'| 'type' | 'macro' | atom().
-type name() :: atom().
-type signature() :: [binary()].
-type doclanguage() :: binary().
-type docvalue() :: binary() | term().
-type mime_type() :: binary().
-type metadata() :: map().
-type doc() :: #{doclanguage() := docvalue()} | none |hidden.
-type doc_entry() :: {{kind(), name(), arity()},
                      erl_anno:anno(),
                      signature(),
                      doc(),
                      metadata()}.
-record(docs_v1, {anno :: erl_anno:anno(),
                  beam_language :: atom(),
                  format :: mime_type(),
                  module_doc :: doc(),
                  metadata = #{} :: metadata(),
                  docs = [] :: [doc_entry()] }).
-type docs_v1() :: #docs_v1{}.
