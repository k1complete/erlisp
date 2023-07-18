-module(elisp_docs).
-export([make_docs_v1/5,
        make_docentry/7]).

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

-spec make_docentry(kind(), name(), arity(), erl_anno:anno(), 
                    signature(), doc(), metadata()) -> doc_entry().
make_docentry(Kind, Name, Arity, Anno, Signature, Doc, Meta) ->
    {{Kind, Name, Arity},
     Anno,
     Signature,
     Doc,
     Meta}.

metadata(DocEntry, Type) ->
    setelement(5, DocEntry, Type).

-spec make_docs_v1(erl_anno:anno(), mime_type(), doc(), map(), [doc_entry()]) -> docs_v1().
make_docs_v1(Anno, Format, Doc, Meta, FunDocs) ->
    #docs_v1{anno = Anno,
             beam_language = 'elisp',
             format = Format,
             module_doc = Doc,
             metadata = Meta,
             docs = FunDocs
      }.
