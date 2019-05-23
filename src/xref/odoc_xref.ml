(*
 * Copyright (c) 2014 Leo White <lpw25@cl.cam.ac.uk>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
type lookup_result_found = Component_table.lookup_result_found =
  { root : Odoc_model.Root.t; hidden : bool }

type lookup_result = Component_table.lookup_unit_result =
  | Forward_reference
  | Found of lookup_result_found
  | Not_found

let core_types = Odoc_model.Predefined.core_types

let core_exceptions = Odoc_model.Predefined.core_exceptions

type resolver = Resolve.resolver

let build_resolver = Resolve.build_resolver

let resolve = Resolve.resolve

let resolve_page = Resolve.resolve_page

type expander = Expand.t

let build_expander = Expand.build_expander

let expand = Expand.expand

module Lookup = Lookup
