open Entity_message

let field_to_string =
  let open Field in
  function
  | Admin -> "admin"
  | AssetId -> "asset identifier"
  | Assignment -> "assignment"
  | AssignmentCount -> "no. assignments"
  | Assignments -> "assignments"
  | Building -> "building"
  | CanceledAt -> "canceled at"
  | City -> "city"
  | Comment -> "comment"
  | Contact -> "contact"
  | ContactEmail -> "contact email address"
  | Contacts -> "contacts"
  | CreatedAt -> "created at"
  | CurrentPassword -> "current password"
  | Database -> "database"
  | DatabaseLabel -> "database label"
  | DatabaseUrl -> "database url"
  | Date -> "date"
  | DateTime -> "date and time"
  | DefaultLanguage -> "default language"
  | Description -> "description"
  | DirectRegistrationDisabled -> "direct registration disabled"
  | Disabled -> "disabled"
  | Distribution -> "distribution"
  | Duration -> "duration"
  | Email -> "email address"
  | EmailAddress -> "email address"
  | EmailAddressUnverified -> "unverified email address"
  | EmailAddressVerified -> "verified email address"
  | EmailSuffix -> "email suffix"
  | End -> "end"
  | Experiment -> "experiment"
  | File -> "file"
  | FileMapping -> "file mapping"
  | FileMimeType -> "mime type"
  | Filename -> "filename"
  | Filesize -> "filesize"
  | Firstname -> "firstname"
  | Host -> "host"
  | I18n -> "translation"
  | Icon -> "icon"
  | Id -> "identifier"
  | InactiveUserDisableAfter -> "disable inactive user after"
  | InactiveUserWarning -> "warn inactive user"
  | Invitation -> "invitation"
  | Invitations -> "invitations"
  | Key -> "key"
  | Label -> "label"
  | Language -> "language"
  | LanguageDe -> "german"
  | LanguageEn -> "english"
  | Lastname -> "lastname"
  | Link -> "link"
  | Location -> "location"
  | LogoType -> "logo type"
  | Mailing -> "mailing"
  | MaxParticipants -> "maximum participants"
  | MinParticipants -> "minimum participants"
  | Name -> "name"
  | NewPassword -> "new password"
  | Operator -> "operator"
  | Overbook -> "overbook"
  | Page -> "page"
  | Participant -> "participant"
  | ParticipantCount -> "number of participants"
  | Participants -> "participants"
  | Participated -> "participated"
  | PartnerLogos -> "partner logos"
  | Password -> "password"
  | PasswordConfirmation -> "password confirmation"
  | Paused -> "paused"
  | Rate -> "rate"
  | RecruitmentChannel -> "recruitment channel"
  | RegistrationDisabled -> "registration disabled"
  | ResentAt -> "resent at"
  | Role -> "role"
  | Room -> "room"
  | Root -> "root"
  | Session -> "session"
  | Sessions -> "sessions"
  | Setting -> "setting"
  | ShowUp -> "show up"
  | SmtpAuthMethod -> "smtp authentication method"
  | SmtpAuthServer -> "smtp authentication server"
  | SmtpPassword -> "smtp password"
  | SmtpPort -> "smtp port"
  | SmtpProtocol -> "smtp protocol"
  | SmtpReadModel -> "smtp read model"
  | SmtpUsername -> "smtp username"
  | SmtpWriteModel -> "smtp write model"
  | SortOrder -> "sort order"
  | Start -> "start"
  | Status -> "status"
  | Street -> "street"
  | Styles -> "styles"
  | Tenant -> "tenant"
  | TenantDisabledFlag -> "disabled"
  | TenantId -> "tenant identifier"
  | TenantLogos -> "tenant logos"
  | TenantMaintenanceFlag -> "maintenance flag"
  | TenantPool -> "Tenant pool"
  | TermsAccepted -> "terms accepted"
  | TermsAndConditions -> "terms and conditions"
  | Time -> "time"
  | TimeSpan -> "time span"
  | Title -> "title"
  | Token -> "token"
  | Translation -> "translation"
  | Url -> "url"
  | User -> "user"
  | Version -> "version"
  | Virtual -> "virtual"
  | WaitingList -> "waiting list"
  | WaitingListDisabled -> "waiting list disabled"
  | Zip -> "zip code"
;;

let info_to_string : info -> string = function
  | Info string -> string
;;

let success_to_string : success -> string = function
  | AddedToWaitingList -> "You were added to the waiting list."
  | AssignmentCreated -> "You have been signed up successfully."
  | Canceled field ->
    field_message "" (field_to_string field) "was successfully canceled."
  | Created field ->
    field_message "" (field_to_string field) "was successfully created."
  | Deleted field ->
    field_message "" (field_to_string field) "was successfully deleted."
  | EmailConfirmationMessage ->
    "Successfully created. An email has been sent to your email address for  \
     verification."
  | EmailVerified -> "Email successfully verified."
  | FileDeleted -> "File was successfully deleted."
  | PasswordChanged -> "Password successfully changed."
  | PasswordReset -> "Password reset, you can now log in."
  | PasswordResetSuccessMessage ->
    "You will receive an email with a link to reset your password if an  \
     account with the provided email is existing."
  | RemovedFromWaitingList -> "You were removed from the waiting list."
  | SentList field ->
    field_message "" (field_to_string field) "were successfully sent."
  | SettingsUpdated -> "Settings were updated successfully."
  | TenantUpdateDatabase -> "Database information was successfully updated."
  | TenantUpdateDetails -> "Tenant was successfully updated."
  | Updated field ->
    field_message "" (field_to_string field) "was successfully updated."
;;

let warning_to_string : warning -> string = function
  | Warning string -> string
;;

let rec error_to_string = function
  | AlreadySignedUpForExperiment ->
    "You are already signed up for this experiment."
  | Conformist errs ->
    CCList.map
      (fun (field, err) ->
        Format.asprintf
          "%s: %s"
          (field_to_string field |> CCString.capitalize_ascii)
          (error_to_string err))
      errs
    |> CCString.concat "\n"
  | ConformistModuleErrorType -> failwith "Do not use"
  | ContactSignupInvalidEmail ->
    "Please provide a valid and unused email address."
  | ContactUnconfirmed -> "Participant isn't confirmed!"
  | Decode field -> field_message "Cannot decode" (field_to_string field) ""
  | DecodeAction -> "Cannot decode action."
  | Disabled field -> field_message "" (field_to_string field) "is disabled."
  | EmailAddressMissingOperator -> "Please provide operator email address."
  | EmailAddressMissingRoot -> "Please provide root email address."
  | EmailAlreadyInUse -> "Email address is already in use."
  | EmailDeleteAlreadyVerified ->
    "Email address is already verified cannot be deleted."
  | EmailMalformed -> "Malformed email"
  | EndBeforeStart -> "End before or equal to start"
  | ExperimentSessionCountNotZero ->
    "Sessions exist for this experiment. It cannot be deleted."
  | HtmxVersionNotFound field ->
    Format.asprintf "No version found for field '%s'" field
  | Invalid field -> field_message "Invalid" (field_to_string field) "provided!"
  | LoginProvideDetails -> "Please provide email and password"
  | MeantimeUpdate field ->
    field_message "" (field_to_string field) "was updated in the meantime!"
  | NegativeAmount -> "Has negative amount!"
  | NoOptionSelected field ->
    field_message "Please select at least one" (field_to_string field) "."
  | NotADatetime (time, err) ->
    Format.asprintf "%s: '%s' is not a valid date or time." err time
  | NotANumber field -> Format.asprintf "'%s' is not a number." field
  | NoTenantsRegistered -> "There are no tenants registered in root database!"
  | NotEligible -> "Your are not eligible to perform this action."
  | NotFound field -> field_message "" (field_to_string field) "not found!"
  | NotFoundList (field, items) ->
    field_message
      "Following"
      (field_to_string field)
      (Format.asprintf "could not be found: %s" (CCString.concat "," items))
  | NotHandled field -> Format.asprintf "Field '%s' is not handled." field
  | NoValue -> "No value provided."
  | PasswordConfirmationDoesNotMatch -> "The provided passwords don't match."
  | PasswordPolicy msg ->
    Format.asprintf "Password doesn't match the required policy! %s" msg
  | PasswordResetFailMessage ->
    "You will receive an email with a link to reset your password if an  \
     account with the provided email is existing."
  | PasswordResetInvalidData -> "Invalid token or password provided"
  | PoolContextNotFound -> "Context could not be found."
  | RegistrationDisabled -> "registration is disabled."
  | RequestRequiredFields -> "Please provide necessary fields"
  | Retrieve field -> field_message "Cannot retrieve" (field_to_string field) ""
  | SessionFullyBooked -> "Session is fully booked"
  | SessionInvalid -> "Invalid session, please login."
  | SessionTenantNotFound ->
    "Something on our side went wrong, please try again later or on multi  \
     occurrences please contact the Administrator."
  | Smaller (field1, field2) ->
    Format.asprintf
      "%s smaller than %s"
      (field_to_string field1)
      (field_to_string field2)
  | TerminatoryTenantError | TerminatoryRootError -> "Please try again later."
  | TerminatoryTenantErrorTitle | TerminatoryRootErrorTitle ->
    "An error occurred"
  | TermsAndConditionsMissing -> "Terms and conditions have to be added first."
  | TermsAndConditionsNotAccepted -> "Terms and conditions not accepted"
  | TimeInPast -> "Time is in the past!"
  | TimeSpanPositive -> "Time span must be positive!"
  | TokenAlreadyUsed -> "The token was already used."
  | TokenInvalidFormat -> "Invalid Token Format!"
  | Undefined field -> field_message "Undefined" (field_to_string field) ""
  | WaitingListFlagsMutuallyExclusive ->
    "Direct registration can only be disabled when waiting list is enabled."
  | WriteOnlyModel -> "Write only model!"
;;

let format_submit submit field =
  let field_opt_message f =
    f |> CCOption.map field_to_string |> CCOption.value ~default:""
  in
  field_message "" submit (field_opt_message field)
;;

let control_to_string = function
  | Accept field -> format_submit "accept" field
  | Add field -> format_submit "add" field
  | AddToWaitingList -> "Sign up for the waiting list"
  | Ascending -> "ascending"
  | Assign field -> format_submit "assign" field
  | Back -> format_submit "back" None
  | Cancel field -> format_submit "cancel" field
  | Choose field -> format_submit "choose" field
  | Create field -> format_submit "create" field
  | Decline -> format_submit "decline" None
  | Delete field -> format_submit "delete" field
  | Descending -> "descending"
  | Disable -> format_submit "disable" None
  | Edit field -> format_submit "edit" field
  | Enable -> format_submit "enable" None
  | Enroll -> format_submit "enroll" None
  | Login -> format_submit "login" None
  | More -> "more"
  | RemoveFromWaitingList -> "Remove from waiting list"
  | Resend field -> format_submit "resend" field
  | Save field -> format_submit "save" field
  | SelectFilePlaceholder -> format_submit "select file.." None
  | Send field -> format_submit "send" field
  | SendResetLink -> format_submit "send reset link" None
  | SignUp -> format_submit "sign up" None
  | Update field -> format_submit "update" field
;;

let to_string = function
  | Message string -> string
  | PageNotFoundMessage -> "The requested page could not be found."
;;
