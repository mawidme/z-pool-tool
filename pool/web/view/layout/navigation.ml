module I18nGuard = I18n.Guard
module NavUtils = Navigation_utils
open CCFun
open Entity

module NavElements = struct
  let read_entity entity =
    Guard.(ValidationSet.One (Action.Read, TargetSpec.Entity entity))
  ;;

  module Profile = struct
    open I18n

    let prefixed ?(prefix = "") = Format.asprintf "%s%s" prefix
    let nav_link = Profile
    let icon = Icon.Person

    let dropdown ?(contact = false) ?prefix () =
      (if contact
       then
         [ "/user/personal-details", PersonalDetails
         ; "/user/contact-information", ContactInformation
         ]
       else [])
      @ [ "/user/login-information", LoginInformation ]
      |> CCList.map (fun (url, field) -> prefixed ?prefix url, field)
      |> NavElement.create_all_req
    ;;

    let element ?contact ?prefix () =
      ( prefixed ?prefix "/user"
      , nav_link
      , Some icon
      , dropdown ?contact ?prefix () )
    ;;

    let nav ?contact ?prefix () =
      NavElement.create
        ~icon
        ~children:(dropdown ?contact ?prefix ())
        (prefixed ?prefix "/user")
        nav_link
    ;;
  end

  let guest ?(root = false) =
    let prefix = if root then Some "root" else None in
    [ NavElement.login ?prefix () ] |> NavUtils.with_language_switch
  ;;

  let contact =
    let open I18n in
    let links =
      [ "/experiments", Experiments, None, []
      ; Profile.element ~contact:true ()
      ]
      |> NavElement.create_all
    in
    links @ [ NavElement.logout () ] |> NavUtils.with_language_switch
  ;;

  let admin =
    let open I18n in
    let settings =
      [ "/admin/custom-fields", CustomFields, Custom_field.Guard.Access.index
      ; "/admin/filter", Filter, Filter.Guard.Access.index
      ; "/admin/locations", Locations, Pool_location.Guard.Access.index
      ; "/admin/settings/queue", Queue, Queue.Guard.Access.index
      ; "/admin/settings", SystemSettings, Settings.Guard.Access.index
      ; "/admin/settings/schedules", Schedules, Schedule.Guard.Access.index
      ; "/admin/settings/smtp", Smtp, Email.Guard.Access.Smtp.index
      ; "/admin/settings/rules", Rules, Guard.Access.manage_rules
      ; "/admin/settings/tags", Tags, Tags.Guard.Access.index
      ; ( "/admin/message-template"
        , MessageTemplates
        , Message_template.Guard.Access.index )
      ; "/admin/i18n", I18n, I18nGuard.Access.index
      ; ( "/admin/organisational-unit"
        , OrganisationalUnits
        , Organisational_unit.Guard.Access.index )
      ]
      |> NavElement.create_all_req_with_set
      |> fun children ->
      let validation_set =
        CCList.map
          (fun { NavElement.validation_set; _ } -> validation_set)
          children
        |> Guard.ValidationSet.or_
      in
      NavElement.create ~validation_set ~children "/admin/settings" Settings
    in
    let user =
      [ "/admin/contacts", Contacts, Contact.Guard.Access.index
      ; "/admin/admins", Admins, Admin.Guard.Access.index
      ]
      |> NavElement.create_all_req_with_set
      |> fun children ->
      NavElement.create
        ~validation_set:
          (Guard.ValidationSet.Or
             [ Contact.Guard.Access.index; Admin.Guard.Access.index ])
        ~children
        "/admin/users"
        Users
    in
    let dashboard = NavElement.create "/admin/dashboard" Dashboard in
    let experiments =
      NavElement.create
        ~validation_set:Experiment.Guard.Access.index
        "/admin/experiments"
        Experiments
    in
    [ dashboard
    ; experiments
    ; settings
    ; user
    ; Profile.nav ~prefix:"/admin" ()
    ; NavElement.logout ()
    ]
    |> NavUtils.create_main ~validate:true
  ;;

  let root =
    let open I18n in
    let tenants =
      NavElement.create
        ~validation_set:Pool_tenant.Guard.Access.index
        "/root/tenants"
        Tenants
    in
    let users =
      NavElement.create
        ~validation_set:Admin.Guard.Access.index
        "/root/users"
        Users
    in
    let settings =
      [ "/root/settings/smtp", Smtp, Email.Guard.Access.Smtp.index ]
      |> NavElement.create_all_req_with_set
      |> fun children -> NavElement.create ~children "/root/settings" Settings
    in
    [ tenants
    ; users
    ; settings
    ; Profile.nav ~prefix:"/root" ()
    ; NavElement.logout ~prefix:"/root" ()
    ]
    |> NavUtils.create_main ~validate:true
  ;;

  let find_tenant_nav_links languages = function
    | Pool_context.Guest -> guest languages
    | Pool_context.Contact _ -> contact languages
    | Pool_context.Admin _ -> admin
  ;;

  let find_root_nav_links languages = function
    | Pool_context.Guest | Pool_context.Contact _ -> guest ~root:true languages
    | Pool_context.Admin _ -> root
  ;;
end

let create
  ?(kind : [ `Tenant | `Root ] = `Tenant)
  ?active_navigation
  database_label
  title
  tenant_languages
  query_language
  active_lang
  user
  =
  let%lwt actor =
    Pool_context.Utils.find_authorizable_opt database_label user
  in
  let nav_links =
    (match kind with
     | `Tenant -> NavElements.find_tenant_nav_links
     | `Root -> NavElements.find_root_nav_links)
      tenant_languages
      user
      ?actor
      ?active_navigation
      database_label
      active_lang
      query_language
  in
  let%lwt desktop = NavUtils.create_desktop nav_links in
  let%lwt mobile = NavUtils.create_mobile title nav_links in
  Lwt.return [ desktop; mobile ]
;;

let create_root ?active_navigation = create ~kind:`Root ?active_navigation