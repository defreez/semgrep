(*s: semgrep/matching/Unit_matcher.ml *)
open Common
open OUnit
module A = AST_generic
module PPG = Pretty_print_generic
module R = Mini_rule

(*****************************************************************************)
(* Semgrep Unit tests *)
(*****************************************************************************)
let test_path = "../../../tests/OTHER/synthesizing/targets/"

(* Format: file, list of target ranges, expected pattern lines.
  For readability, the pattern lines are a list. The pattern is
  the newline delimited concatenation of the list of lines.
*)

let statement_list_tests =
  [
    ("equal_length_assign_call.py", [ "1:0-2:5"; "4:0-5:5" ], "$X = a\nfoo($X)");
    ( "equal_length_assign_call.js",
      [ "1:0-2:7"; "4:0-5:7" ],
      "$X = req.query.foo;\nexec($X);" );
  ]

(* Range.t does not derive eq *)
let compare_range (r1 : Range.t) (r2 : Range.t) : bool =
  r1.start == r2.start && r1.end_ == r2.end_

let parse_file lang file : AST_generic.program =
  let { Parse_target.ast; errors; _ } =
    Parse_target.parse_and_resolve_name_use_pfff_or_treesitter lang file
  in
  if errors <> [] then failwith (spf "fail to parse %s" file) else ast

let extract_range (m : Pattern_match.t) : Range.t =
  let sloc, eloc = m.range_loc in
  Range.range_of_token_locations sloc eloc

(* Evaluates to the ranges that a pattern matches on a file. *)
let ranges_matched lang file pattern : Range.t list =
  let ast = parse_file lang file in
  let rule =
    {
      R.id = "unit testing";
      pattern;
      message = "";
      severity = R.Error;
      languages = [ lang ];
      pattern_string = "test: no need for pattern string";
    }
  in
  let equiv = [] in
  (* Are equivalences necessary for this? *)
  let matches =
    Match_patterns.check
      ~hook:(fun _env matched_tokens ->
        let xs = Lazy.force matched_tokens in
        let toks = xs |> List.filter Parse_info.is_origintok in
        let minii, _maxii = Parse_info.min_max_ii_by_pos toks in
        Error_code.error minii (Error_code.SemgrepMatchFound ("", "")))
      Config_semgrep.default_config [ rule ] equiv (file, lang, ast)
  in
  List.map extract_range matches

let single_test file linecols expected_pattern =
  let ranges_expected =
    List.map (fun lcs -> Range.range_of_linecol_spec lcs file) linecols
  in
  let lang, _, inferred_pattern =
    Synthesizer.generate_pattern_from_targets Config_semgrep.default_config
      (linecols @ [ file ])
  in
  let ranges_actual = ranges_matched lang file inferred_pattern in
  let actual_pattern =
    Pretty_print_generic.pattern_to_string lang inferred_pattern
  in
  let ranges_correct =
    List.for_all2 compare_range ranges_expected ranges_actual
  in
  let pattern_correct = actual_pattern = expected_pattern in
  assert_bool "ranges should match" ranges_correct;
  assert_bool
    (spf "actual pattern:\n%s\n\nexpected pattern:\n%s\n" actual_pattern
       expected_pattern)
    pattern_correct

let unittest =
  "pattern from targets" >:: fun () ->
  statement_list_tests
  |> List.iter (fun (file, linecols, expected_pattern) ->
         single_test (test_path ^ file) linecols expected_pattern)
