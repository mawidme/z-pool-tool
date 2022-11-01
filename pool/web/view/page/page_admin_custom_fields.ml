open Tyxml.Html
open Component
module Message = Pool_common.Message

module Url = struct
  open Custom_field

  let concat = CCString.concat "/"
  let fallback_path = "/admin/custom-fields"
  let index_path m = [ fallback_path; Model.show m ] |> concat

  module Field = struct
    let field_key = "field"
    let create_path m = [ index_path m; field_key ] |> concat
    let new_path m = [ index_path m; field_key; "new" ] |> concat

    let detail_path (m, id) =
      [ index_path m; field_key; id |> Id.value ] |> concat
    ;;

    let edit_path (m, id) = [ detail_path (m, id); "edit" ] |> concat
  end

  module Option = struct
    open SelectOption

    let options_key = "options"
    let index_path = Field.detail_path
    let new_path field = [ index_path field; options_key; "new" ] |> concat
    let create_path field = [ Field.detail_path field; options_key ] |> concat

    let detail_path field id =
      [ index_path field; options_key; id |> Id.value ] |> concat
    ;;

    let edit_path field id = [ detail_path field id; "edit" ] |> concat
    let delete_path field id = [ detail_path field id; "delete" ] |> concat
  end

  module Group = struct
    open Group

    let group_key = "group"
    let index_path = index_path
    let new_path m = [ index_path m; group_key; "new" ] |> concat
    let create_path m = [ index_path m; group_key ] |> concat

    let detail_path (model, id) =
      [ index_path model; group_key; id |> Id.value ] |> concat
    ;;

    let edit_path group = [ detail_path group; "edit" ] |> concat
    let delete_path group = [ detail_path group; "delete" ] |> concat
  end
end

let model_subtitle language model =
  div
    [ p
        [ strong
            [ txt
                (Format.asprintf
                   "%s: "
                   Pool_common.(
                     Utils.field_to_string language Message.Field.Model
                     |> CCString.capitalize_ascii))
            ]
        ; txt (Custom_field.Model.show model |> CCString.capitalize_ascii)
        ]
    ]
;;

let custom_fields_layout language current_model html =
  let open Custom_field in
  let subnav_links =
    Model.(
      all
      |> CCList.map (fun f ->
           ( f |> show |> CCString.capitalize_ascii
           , f |> Url.index_path
           , equal current_model f )))
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin"; "measure" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt Pool_common.(Utils.nav_link_to_string language I18n.CustomFields)
        ]
    ; Component.Navigation.make_subnav subnav_links
    ; h2
        ~a:[ a_class [ "heading-2" ] ]
        [ txt (current_model |> Model.show |> CCString.capitalize_ascii) ]
    ; div
        ~a:[ a_class [ "stack" ] ]
        [ div
            ~a:[ a_class [ "flexrow"; "flex-gap" ] ]
            [ a
                ~a:
                  [ a_href
                      (Url.Field.new_path current_model
                      |> Sihl.Web.externalize_path)
                  ]
                [ txt
                    Pool_common.(
                      Message.(Add (Some Field.CustomField))
                      |> Utils.control_to_string language)
                ]
            ; a
                ~a:
                  [ a_href
                      (Url.Group.new_path current_model
                      |> Sihl.Web.externalize_path)
                  ]
                [ txt
                    Pool_common.(
                      Message.(Add (Some Field.CustomFieldGroup))
                      |> Utils.control_to_string language)
                ]
            ]
        ; html
        ]
    ]
;;

let input_by_lang
  ?(required = false)
  language
  tenant_languages
  flash_fetcher
  elm
  field
  value_fnc
  =
  let open Pool_common in
  let group_class = Elements.group_class [] `Horizontal in
  CCList.map
    (fun lang ->
      let required =
        required
        && CCList.mem ~eq:Pool_common.Language.equal lang tenant_languages
      in
      let label_text =
        lang
        |> Language.field_of_t
        |> Utils.field_to_string language
        |> CCString.capitalize_ascii
      in
      let id =
        Format.asprintf "%s-%s" (Message.Field.show field) (Language.show lang)
      in
      let value =
        let open CCOption in
        flash_fetcher
          (Format.asprintf
             "%s[%s]"
             (Message.Field.show field)
             (Language.show lang))
        <+> (elm >|= value_fnc lang)
        |> value ~default:""
      in
      let input_element =
        let attrs =
          [ a_input_type `Text
          ; a_id id
          ; a_name
              (Format.asprintf
                 "%s[%s]"
                 (Message.Field.show field)
                 (Language.show lang))
          ; a_value value
          ]
        in
        div
          ~a:[ a_class [ "input-group" ] ]
          [ input ~a:(if required then a_required () :: attrs else attrs) () ]
      in
      div
        ~a:[ a_class group_class ]
        [ label
            ~a:[ a_label_for id ]
            [ txt
                (if required
                then Format.asprintf "%s *" label_text
                else label_text)
            ]
        ; input_element
        ])
    Pool_common.Language.all
;;

let field_form
  ?(custom_field : Custom_field.t option)
  current_model
  Pool_context.{ language; csrf; _ }
  groups
  tenant_languages
  flash_fetcher
  =
  let open Custom_field in
  let action =
    match custom_field with
    | None -> Url.Field.create_path current_model
    | Some f -> Url.Field.detail_path (model f, id f)
  in
  let checkbox_element
    ?(disabled = false)
    ?orientation
    ?help
    ?(default = false)
    field
    fnc
    =
    checkbox_element
      language
      ~additional_attributes:(if disabled then [ a_disabled () ] else [])
      ?orientation
      ?help
      field
      ~value:(custom_field |> CCOption.map_or ~default fnc)
      ~flash_fetcher
  in
  let value = CCFun.flip (CCOption.map_or ~default:"") custom_field in
  let field_type_opt = CCOption.map field_type custom_field in
  let input_by_lang ?required =
    input_by_lang ?required language tenant_languages flash_fetcher custom_field
  in
  let name_inputs =
    input_by_lang ~required:true Message.Field.Name (fun lang f ->
      let open CCOption in
      f |> name |> Name.find_opt lang >|= Name.value_name |> value ~default:"")
  in
  let hint_inputs =
    input_by_lang Message.Field.Hint (fun lang f ->
      let open CCOption in
      f |> hint |> Hint.find_opt lang >|= Hint.value_hint |> value ~default:"")
  in
  let validation_subform =
    let current_values =
      custom_field
      |> CCOption.map_or ~default:[] (fun f -> f |> validation_strings)
    in
    let rule_input field_type name input_type value disabled =
      let prefixed_name =
        Format.asprintf "%s[%s]" Message.Field.(show Validation) name
      in
      let wrapper_class = [ "switcher"; "flex-gap"; "align-center" ] in
      let input_attributes =
        [ a_input_type input_type
        ; a_name prefixed_name
        ; a_id name
        ; a_value value
        ; a_class [ "grow-2" ]
        ]
      in
      let attrs, classes =
        match disabled with
        | true -> a_disabled () :: input_attributes, "disabled" :: wrapper_class
        | false -> input_attributes, wrapper_class
      in
      div
        ~a:
          [ a_class classes
          ; a_user_data "field-type" (FieldType.show field_type)
          ]
        [ div
            ~a:[ a_class [ "grow" ] ]
            [ label ~a:[ a_label_for name ] [ txt name ] ]
        ; input ~a:attrs ()
        ]
    in
    let functions =
      {js|
        var select = document.querySelector("[name='field_type']");
        select.addEventListener("change", function(e) {
          var type = e.currentTarget.value;
          var inputs = document.querySelectorAll("[data-field-type]");
          inputs.forEach(function(elm){
            if(elm.dataset.fieldType != type) {
              elm.classList.add("disabled")
            } else {
              elm.classList.remove("disabled")
            }
            var input = elm.querySelector('input');
            input.disabled = elm.dataset.fieldType != type;
          })
        })
    |js}
    in
    div
      [ div
          ~a:[ a_class [ "flexcolumn"; "stack" ] ]
          (CCList.map
             (fun (key, input_type, validation_type) ->
               let value =
                 CCList.assoc_opt ~eq:CCString.equal key current_values
                 |> CCOption.value ~default:""
               in
               let disabled =
                 field_type_opt
                 |> CCOption.map_or ~default:false (fun t ->
                      FieldType.equal validation_type t |> not)
               in
               rule_input validation_type key input_type value disabled)
             Validation.all)
      ; script (Unsafe.data functions)
      ]
  in
  let select_options_html =
    let empty = txt "" in
    match custom_field with
    | None -> empty
    | Some m ->
      (match m with
       | Select (_, options) | MultiSelect (_, options) ->
         let list =
           form
             ~a:
               [ a_method `Post
               ; a_action
                   (Sihl.Web.externalize_path
                      (Url.Field.detail_path (model m, id m)
                      |> Format.asprintf "%s/sort-options"))
               ; a_class [ "stack" ]
               ]
             (CCList.cons
                (div
                   ~a:[ a_user_data "sortable" "" ]
                   (CCList.map
                      (fun option ->
                        div
                          ~a:
                            [ a_class
                                [ "flexrow"
                                ; "flex-gap"
                                ; "justify-between"
                                ; "align-center"
                                ]
                            ; a_user_data "sortable-item" ""
                            ; a_draggable true
                            ]
                          [ div [ txt (SelectOption.name language option) ]
                          ; div
                              [ input
                                  ~a:
                                    [ a_input_type `Hidden
                                    ; a_name
                                        Message.Field.(
                                          CustomFieldOption |> array_key)
                                    ; a_value SelectOption.(Id.value option.id)
                                    ]
                                  ()
                              ]
                          ; div
                              ~a:
                                [ a_class
                                    [ "flexrow"; "flex-gap"; "align-center" ]
                                ]
                              [ a
                                  ~a:
                                    [ a_href
                                        (Url.Option.edit_path
                                           (model m, id m)
                                           option.SelectOption.id
                                        |> Sihl.Web.externalize_path)
                                    ]
                                  [ txt
                                      Pool_common.(
                                        Message.(Edit None)
                                        |> Utils.control_to_string language)
                                  ]
                              ]
                          ])
                      options))
                [ csrf_element csrf ()
                ; submit_element
                    language
                    Message.UpdateOrder
                    ~submit_type:`Success
                    ()
                ])
         in
         div
           [ h2
               ~a:[ a_class [ "heading-2" ] ]
               [ txt
                   (Message.Field.CustomFieldOption
                   |> Pool_common.Utils.field_to_string language
                   |> CCString.capitalize_ascii)
               ]
           ; p
               [ a
                   ~a:
                     [ a_href
                         (Url.Option.new_path (model m, id m)
                         |> Sihl.Web.externalize_path)
                     ]
                   [ txt
                       Pool_common.(
                         Message.(Add (Some Field.CustomFieldOption))
                         |> Utils.control_to_string language)
                   ]
               ]
           ; list
           ]
       | Boolean _ | Number _ | Text _ -> empty)
  in
  [ model_subtitle language current_model
  ; form
      ~a:
        [ a_method `Post
        ; a_action (Sihl.Web.externalize_path action)
        ; a_class [ "stack-lg" ]
        ]
      [ csrf_element csrf ()
      ; div
          ~a:[ a_class [ "switcher"; "flex-gap" ] ]
          [ selector
              language
              Message.Field.FieldType
              FieldType.show
              FieldType.all
              field_type_opt
              ~option_formatter:FieldType.to_string
              ~add_empty:true
              ~required:true
              ~flash_fetcher
              ()
          ; Group.(
              selector
                language
                Message.Field.CustomFieldGroup
                Group.(fun (g : t) -> g.id |> Id.value)
                groups
                Custom_field.(
                  let open CCOption in
                  custom_field
                  >>= group_id
                  >>= fun id ->
                  CCList.find_opt Group.(fun (g : t) -> Id.equal g.id id) groups)
                ~option_formatter:(name language)
                ~add_empty:true
                ~flash_fetcher
                ())
          ]
      ; div
          ~a:[ a_class [ "stack" ] ]
          [ h4
              ~a:[ a_class [ "heading-4" ] ]
              [ txt
                  Pool_common.(
                    Message.Field.Name
                    |> Utils.field_to_string language
                    |> CCString.capitalize_ascii)
              ]
          ; div ~a:[ a_class [ "stack" ] ] name_inputs
          ]
      ; div
          ~a:[ a_class [ "stack" ] ]
          [ h4
              ~a:[ a_class [ "heading-4" ] ]
              [ txt
                  Pool_common.(
                    Message.Field.Hint
                    |> Utils.field_to_string language
                    |> CCString.capitalize_ascii)
              ]
          ; div ~a:[ a_class [ "stack" ] ] hint_inputs
          ]
      ; div
          ~a:[ a_class [ "stack" ] ]
          [ h4
              ~a:[ a_class [ "heading-4" ] ]
              [ txt
                  Pool_common.(I18n.Validation |> Utils.text_to_string language)
              ]
          ]
      ; validation_subform
      ; div
          ~a:[ a_class [ "stack" ] ]
          [ h4
              ~a:[ a_class [ "heading-4" ] ]
              [ txt
                  Pool_common.(
                    Message.Field.Admin
                    |> Utils.field_to_string language
                    |> CCString.capitalize_ascii)
              ]
          ; input_element
              language
              `Text
              Message.Field.AdminHint
              ~orientation:`Horizontal
              ~value:
                (value (fun f ->
                   (f |> admin).Admin.hint
                   |> CCOption.map_or ~default:"" Admin.Hint.value))
              ~flash_fetcher
          ; checkbox_element Message.Field.Overwrite (fun f ->
              (f |> admin).Admin.overwrite |> Admin.Overwrite.value)
          ; checkbox_element
              ~disabled:
                (custom_field
                |> CCOption.map_or ~default:false (fun f ->
                     (f |> admin).Admin.view_only |> Admin.ViewOnly.value))
              ~help:Pool_common.I18n.CustomFieldAdminInputOnly
              Message.Field.AdminInputOnly
              (fun f -> (f |> admin).Admin.input_only |> Admin.InputOnly.value)
          ; checkbox_element
              ~help:Pool_common.I18n.CustomFieldAdminViewOnly
              Message.Field.AdminViewOnly
              (fun f -> (f |> admin).Admin.view_only |> Admin.ViewOnly.value)
          ]
      ; div
          ~a:[ a_class [ "stack" ] ]
          [ checkbox_element
              ~disabled:
                (custom_field
                |> CCOption.map_or ~default:false (fun f ->
                     (f |> admin).Admin.input_only |> Admin.InputOnly.value
                     || FieldType.(equal (f |> field_type) MultiSelect)))
              Message.Field.Required
              (fun f -> f |> required |> Required.value)
          ; checkbox_element Message.Field.Disabled (fun f ->
              f |> disabled |> Disabled.value)
          ; submit_element
              language
              Message.(
                let field = Some Field.CustomField in
                match custom_field with
                | None -> Create field
                | Some _ -> Update field)
              ~submit_type:`Success
              ()
          ]
      ; (Format.asprintf
           {js|
              var adminViewOnly = document.querySelector("[name='%s']");
              var adminInputOnly = document.querySelector("[name='%s']");
              var required = document.querySelector("[name='%s']");
              var fieldType = document.querySelector("[name='%s']");

              function triggerEvent(elm, name) {
                var event = document.createEvent("HTMLEvents");
                event.initEvent(name, false, true);
                elm.dispatchEvent(event);
              }

              adminInputOnly.addEventListener("change", function(e) {
                required.disabled = e.currentTarget.checked;
              })

              adminViewOnly.addEventListener("change", function(e) {
                adminInputOnly.disabled = e.currentTarget.checked;
                if(e.currentTarget.checked) {
                  adminInputOnly.checked = true;
                  triggerEvent(adminInputOnly, 'change');
                }
              })

              fieldType.addEventListener("change", function(e) {
                required.disabled = (e.currentTarget.value === "%s")
              })
         |js}
           Message.Field.(show AdminViewOnly)
           Message.Field.(show AdminInputOnly)
           Message.Field.(show Required)
           Message.Field.(show FieldType)
           FieldType.(show MultiSelect)
        |> fun js -> script (Unsafe.data js))
      ]
  ; select_options_html
  ]
;;

let detail
  ?custom_field
  current_model
  (Pool_context.{ language; _ } as context)
  groups
  sys_languages
  flash_fetcher
  =
  div
    ~a:[ a_class [ "trim"; "safety-margin"; "measure" ] ]
    [ Partials.form_title language Message.Field.CustomField custom_field
    ; div
        ~a:[ a_class [ "stack-lg" ] ]
        (field_form
           ?custom_field
           current_model
           context
           groups
           sys_languages
           flash_fetcher)
    ]
;;

let index field_list group_list current_model Pool_context.{ language; csrf; _ }
  =
  let thead = Message.Field.[ Some Title; Some CustomFieldGroup; None ] in
  let rows =
    let open Custom_field in
    let open CCOption in
    CCList.map
      (fun field ->
        [ txt
            (field
            |> name
            |> Name.find_opt language
            |> map_or ~default:"-" Name.value_name)
        ; txt
            (field
            |> group_id
            >>= (fun id ->
                  CCList.find_opt
                    Group.(fun (g : t) -> Id.equal g.id id)
                    group_list
                  >|= Group.name language)
            |> value ~default:"")
        ; a
            ~a:
              [ a_href
                  (Url.Field.edit_path (model field, id field)
                  |> Sihl.Web.externalize_path)
              ]
            [ txt Pool_common.(Message.More |> Utils.control_to_string language)
            ]
        ])
      field_list
  in
  let groups_html =
    let list =
      form
        ~a:
          [ a_method `Post
          ; a_action
              (Url.Group.index_path current_model
              |> Format.asprintf "%s/group/sort"
              |> Sihl.Web.externalize_path)
          ; a_class [ "stack" ]
          ]
        (CCList.cons
           (div
              ~a:[ a_user_data "sortable" "" ]
              (CCList.map
                 (fun group ->
                   let open Custom_field in
                   div
                     ~a:
                       [ a_class
                           [ "flexrow"
                           ; "flex-gap"
                           ; "justify-between"
                           ; "align-center"
                           ]
                       ; a_user_data "sortable-item" ""
                       ; a_draggable true
                       ]
                     [ div [ txt Group.(group |> name language) ]
                     ; div
                         [ input
                             ~a:
                               [ a_input_type `Hidden
                               ; a_name
                                   Message.Field.(CustomFieldGroup |> array_key)
                               ; a_value Group.(Id.value group.id)
                               ]
                             ()
                         ]
                     ; div
                         ~a:
                           [ a_class [ "flexrow"; "flex-gap"; "align-center" ] ]
                         [ a
                             ~a:
                               [ a_href
                                   (Url.Group.edit_path
                                      Group.(group.model, group.id)
                                   |> Sihl.Web.externalize_path)
                               ]
                             [ txt
                                 Pool_common.(
                                   Message.(Edit None)
                                   |> Utils.control_to_string language)
                             ]
                         ]
                     ])
                 group_list))
           [ csrf_element csrf ()
           ; submit_element
               language
               Message.UpdateOrder
               ~submit_type:`Success
               ()
           ])
    in
    div
      [ h2
          ~a:[ a_class [ "heading-2" ] ]
          [ txt
              (Message.Field.CustomFieldGroup
              |> Pool_common.Utils.field_to_string language
              |> CCString.capitalize_ascii)
          ]
      ; p
          [ a
              ~a:
                [ a_href
                    (Url.Group.new_path current_model
                    |> Sihl.Web.externalize_path)
                ]
              [ txt
                  Pool_common.(
                    Message.(Create (Some Field.CustomFieldGroup))
                    |> Utils.control_to_string language)
              ]
          ]
      ; list
      ]
  in
  div
    ~a:[ a_class [ "stack-lg" ] ]
    [ Table.horizontal_table `Striped language ~thead rows; groups_html ]
  |> custom_fields_layout language current_model
;;