-module(elisp_docs).
-export([make_docs_v1/5,
         make_docentry/7,
         add_docentry/2,
         metadata/2,
         set_moduledoc/2,
         new_docs_v1/0]).

-include_lib("elisp_docs.hrl").

-spec new_docs_v1() -> #docs_v1{}.
new_docs_v1() ->
    #docs_v1{}.

-spec set_moduledoc(#docs_v1{}, #docs_v1{}) -> #docs_v1{}.
set_moduledoc(D, M) ->
    Docentry = D#docs_v1.docs,
    DDocentry = M#docs_v1.docs,
    M#docs_v1{docs=Docentry++DDocentry}.

-spec add_docentry(#docs_v1{}, binary()) -> #docs_v1{}.
add_docentry(D, E) ->
    D#docs_v1{docs=[E|D#docs_v1.docs]}.

-spec make_docentry(kind(), name(), arity(), erl_anno:anno(), 
                    signature(), doc(), metadata()) -> doc_entry().
make_docentry(Kind, Name, Arity, Anno, Signature, Doc, Meta) ->
    {{Kind, Name, Arity},
     Anno,
     Signature,
     Doc,
     Meta}.

-spec metadata(doc_entry(), any()) -> doc_entry().
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
