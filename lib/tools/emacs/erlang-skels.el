;;
;; %CopyrightBegin%
;;
;; Copyright Ericsson AB 2010-2024. All Rights Reserved.
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;
;; %CopyrightEnd%
;;;
;;; Purpose: Provide Erlang code skeletons.
;;; See 'erlang-skel-file' variable.

(defvar erlang-tempo-tags nil
  "Tempo tags for erlang mode")

(defvar erlang-skel
  '(("If"            "if"            erlang-skel-if)
    ("Case"          "case"          erlang-skel-case)
    ("Receive"       "receive"       erlang-skel-receive)
    ("Receive After" "after"         erlang-skel-receive-after)
    ("Receive Loop"  "loop"          erlang-skel-receive-loop)
    ("Module"        "module"        erlang-skel-module)
    ("Author"        "author"        erlang-skel-author)
    ("Function"      "function"      erlang-skel-function)
    ("Spec"          "spec"          erlang-skel-spec)
    ()
    ("Small Header"  "small-header"
     erlang-skel-small-header erlang-skel-header)
    ("Normal Header" "normal-header"
     erlang-skel-normal-header erlang-skel-header)
    ("Large Header"  "large-header"
     erlang-skel-large-header erlang-skel-header)
    ()
    ("Small Server"   "small-server"
     erlang-skel-small-server erlang-skel-header)
    ()
    ("Application" "application"
     erlang-skel-application erlang-skel-header)
    ("Supervisor" "supervisor"
     erlang-skel-supervisor erlang-skel-header)
    ("supervisor_bridge" "supervisor-bridge"
     erlang-skel-supervisor-bridge erlang-skel-header)
    ("gen_server" "generic-server"
     erlang-skel-generic-server erlang-skel-header)
    ("gen_event" "gen-event"
     erlang-skel-gen-event erlang-skel-header)
    ("gen_fsm" "gen-fsm"
     erlang-skel-gen-fsm erlang-skel-header)
    ("gen_statem (StateName/3)" "gen-statem-StateName"
     erlang-skel-gen-statem-StateName erlang-skel-header)
    ("gen_statem (handle_event/4)" "gen-statem-handle-event"
     erlang-skel-gen-statem-handle-event erlang-skel-header)
    ("wx_object" "wx-object"
     erlang-skel-wx-object erlang-skel-header)
    ("Library module" "gen-lib"
     erlang-skel-lib erlang-skel-header)
    ("Corba callback" "gen-corba-cb"
     erlang-skel-corba-callback erlang-skel-header)
    ("Small Common Test suite" "ct-test-suite-s"
     erlang-skel-ct-test-suite-s erlang-skel-header)
    ("Large Common Test suite" "ct-test-suite-l"
     erlang-skel-ct-test-suite-l erlang-skel-header)
    ("Common Test Hook" "ct-hook"
     erlang-skel-ct-hook erlang-skel-header)
    ("Erlang TS test suite" "ts-test-suite"
     erlang-skel-ts-test-suite erlang-skel-header)
  )
  "*Description of all skeleton templates.
Both functions and menu entries will be created.

Each entry in `erlang-skel' should be a list with three or four
elements, or the empty list.

The first element is the name which shows up in the menu.  The second
is the `tempo' identifier (The string \"erlang-\" will be added in
front of it).  The third is the skeleton descriptor, a variable
containing `tempo' attributes as described in the function
`tempo-define-template'.  The optional fourth elements denotes a
function which should be called when the menu is selected.

Functions corresponding to every template will be created.  The name
of the function will be `tempo-template-erlang-X' where `X' is the
tempo identifier as specified in the second argument of the elements
in this list.

A list with zero elements means that the a horizontal line should
be placed in the menu.")

(defvar erlang-skel-use-separators t
  "A boolean than determines whether the skeletons include horizontal
separators.

Should this variable be nil, the documentation for functions will not
include separators of the form %%--...")

;; In XEmacs `user-mail-address' returns "x@y.z (Foo Bar)" ARGH!
;; What's wrong with that? RFC 822 says it's legal.   [sverkerw]
;; This needs to use the customized value.  If that's not sane, things like
;; add-log will lose anyhow.  Avoid it if there _is_ a paren.
(defvar erlang-skel-mail-address
  (if (or (not user-mail-address) (string-match "(" user-mail-address))
      (concat (user-login-name) "@"
              (or (and (boundp 'mail-host-address)
                       mail-host-address)
                  (system-name)))
    user-mail-address)
  "Mail address of the user.")

;; Expression templates:
(defvar erlang-skel-case
  '((erlang-skel-skip-blank) o >
    "case " p " of" n> p "_ ->" n> p "ok" n "end" > p)
  "*The skeleton of a `case' expression.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-if
  '((erlang-skel-skip-blank) o >
    "if"  n> p " ->" n> p "ok" n "end" > p)
  "The skeleton of an `if' expression.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-receive
  '((erlang-skel-skip-blank) o >
    "receive" n> p "_ ->" n> p "ok" n "end" > p)
  "*The skeleton of a `receive' expression.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-receive-after
  '((erlang-skel-skip-blank) o >
    "receive" n> p "_ ->" n> p "ok" n "after " > p "T ->" n>
    p "ok" n "end" > p)
  "*The skeleton of a `receive' expression with an `after' clause.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-receive-loop
  '(& o "loop(" p ") ->" n> "receive" n> p "_ ->" n>
      "loop(" p ")" n "end." >)
  "*The skeleton of a simple `receive' loop.
Please see the function `tempo-define-template'.")


(defvar erlang-skel-function
  '((erlang-skel-separator-start 2)
    "%% @doc" n
    "%% @spec" n
    (erlang-skel-separator-end 2))
    "*The template of a function skeleton.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-spec
  '("-spec " (erlang-skel-get-function-name) "(" (erlang-skel-get-function-args) ") -> undefined.")
    "*The template of a -spec for the function following point.
Please see the function `tempo-define-template'.")

;; Attribute templates

(defvar erlang-skel-module
  '(& "-module("
      (erlang-add-quotes-if-needed (erlang-get-module-from-file-name))
      ")." n)
  "*The skeleton of a `module' attribute.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-author
  '(& "-author('" erlang-skel-mail-address "')." n)
  "*The skeleton of a `author' attribute.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-vc nil
  "*The skeleton template to generate a version control attribute.
The default is to insert nothing.  Example of usage:

    (setq erlang-skel-vc \\='(& \"-rcs(\\\"$\Id: $ \\\").\") n)

Please see the function `tempo-define-template'.")

(defvar erlang-skel-export
  '(& "-export([" n> "])." n)
  "*The skeleton of an `export' attribute.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-import
  '(& "%%-import(Module, [Function/Arity, ...])." n)
  "*The skeleton of an `import' attribute.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-compile nil
  ;;  '(& "%%-compile(export_all)." n)
  "*The skeleton of a `compile' attribute.
Please see the function `tempo-define-template'.")


;; Comment templates.

(defvar erlang-skel-date-function 'erlang-skel-dd-mmm-yyyy
  "*Function which returns date string.
Look in the module `time-stamp' for a battery of functions.")

(defvar erlang-skel-copyright-comment
  (if (boundp '*copyright-organization*)
      '(& "%%% @copyright (C) " (format-time-string "%Y") ", "
          *copyright-organization*  n)
      '(& "%%% @copyright (C) " (format-time-string "%Y") ", "
          (user-full-name)  n))
  "*The template for a copyright line in the header, normally empty.
This variable should be bound to a `tempo' template, for example:
  \\='(& \"%%% Copyright (C) 2000, Yoyodyne, Inc.\" n)
Please see the function `tempo-define-template'.")

(defvar erlang-skel-created-comment
  '(& "%%% Created : " (funcall erlang-skel-date-function) " by "
      (user-full-name) " <" erlang-skel-mail-address ">" n)
  "*The template for the \"Created:\" comment line.")

(defvar erlang-skel-author-comment
  '(& "%%% @author " (user-full-name) " <" erlang-skel-mail-address ">" n)
  "*The template for creating the \"Author:\" line in the header.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-small-header
  '(o (erlang-skel-include erlang-skel-module)
      n
      (erlang-skel-include erlang-skel-compile erlang-skel-vc))
  "*The template of a small header without any comments.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-normal-header
  '(o (erlang-skel-include  erlang-skel-author-comment)
      (erlang-skel-include erlang-skel-copyright-comment)
      "%%% @doc"  n
      "%%%" p n
      "%%% @end" n
      (erlang-skel-include erlang-skel-created-comment) n
      (erlang-skel-include erlang-skel-small-header) n)
  "*The template of a normal header.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-large-header
  '(o (erlang-skel-separator)
      (erlang-skel-include erlang-skel-author-comment)
      (erlang-skel-include erlang-skel-copyright-comment)
      "%%% @doc" n
      "%%%" p n
      "%%% @end" n
      (erlang-skel-include erlang-skel-created-comment)
      (erlang-skel-separator)
      (erlang-skel-include erlang-skel-small-header) )
  "*The template of a large header.
Please see the function `tempo-define-template'.")


 ;; Server templates.
(defvar erlang-skel-small-server
  '((erlang-skel-include erlang-skel-large-header)
    "-export([start/0, init/1])." n n n
    "start() ->" n> "spawn(" (erlang-get-module-from-file-name)
    ", init, [self()])." n n
    "init(From) ->" n>
    "loop(From)." n n
    "loop(From) ->" n>
    "receive" n>
    p "_ ->" n>
    "loop(From)" n
    "end." > n
    )
  "*Template of a small server.
Please see the function `tempo-define-template'.")

;; Behaviour templates.
(defvar erlang-skel-application
  '((erlang-skel-include erlang-skel-large-header)
    "-behaviour(application)." n n
    "%% Application callbacks" n
    "-export([start/2, start_phase/3, stop/1, prep_stop/1," n>
    "config_change/3])." n n
    (erlang-skel-double-separator-start 3)
    "%%% Application callbacks" n
    (erlang-skel-double-separator-end 3) n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% This function is called whenever an application is started using" n
    "%% application:start/[1,2], and should start the processes of the" n
    "%% application. If the application is structured according to the OTP" n
    "%% design principles as a supervision tree, this means starting the" n
    "%% top supervisor of the tree." n
    (erlang-skel-separator-end 2)
    "-spec start(StartType :: normal |" n>
    "{takeover, Node :: node()} |" n>
    "{failover, Node :: node()}," n>
    "StartArgs :: term()) ->" n>
    "{ok, Pid :: pid()} |" n>
    "{ok, Pid :: pid(), State :: term()} |" n>
    "{error, Reason :: term()}." n
    "start(_StartType, _StartArgs) ->" n>
    "case 'TopSupervisor':start_link() of" n>
    "{ok, Pid} ->" n>
    "{ok, Pid};" n>
    "Error ->" n>
    "Error" n
    "end." > n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% top supervisor of the tree." n
    "%% Starts an application with included applications, when" n
    "%% synchronization is needed between processes in the different" n
    "%% applications during startup."
    (erlang-skel-separator-end 2)
    "-spec start_phase(Phase :: atom()," n>
    "StartType :: normal |" n>
    "{takeover, Node :: node()} |" n>
    "{failover, Node :: node()}," n>
    "PhaseArgs :: term()) -> ok | {error, Reason :: term()}." n
    "start_phase(_Phase, _StartType, _PhaseArgs) ->" n>
    "ok."n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% This function is called whenever an application has stopped. It" n
    "%% is intended to be the opposite of Module:start/2 and should do" n
    "%% any necessary cleaning up. The return value is ignored." n
    (erlang-skel-separator-end 2)
    "-spec stop(State :: term()) -> any()." n
    "stop(_State) ->" n>
    "ok." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% This function is called when an application is about to be stopped," n
    "%% before shutting down the processes of the application." n
    (erlang-skel-separator-end 2)
    "-spec prep_stop(State :: term()) -> NewState :: term()." n
    "prep_stop(State) ->" n>
    "State." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% This function is called by an application after a code replacement," n
    "%% if the configuration parameters have changed." n
    (erlang-skel-separator-end 2)
    "-spec config_change(Changed :: [{Par :: atom(), Val :: term()}]," n>
    "New :: [{Par :: atom(), Val :: term()}]," n>
    "Removed :: [Par :: atom()]) -> ok." n
    "config_change(_Changed, _New, _Removed) ->" n>
    "ok." n
    n
    (erlang-skel-double-separator-start 3)
    "%%% Internal functions" n
    (erlang-skel-double-separator-end 3)
    )
  "*The template of an application behaviour.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-supervisor
  '((erlang-skel-include erlang-skel-large-header)
    "-behaviour(supervisor)." n n

    "%% API" n
    "-export([start_link/0])." n n

    "%% Supervisor callbacks" n
    "-export([init/1])." n n

    "-define(SERVER, ?MODULE)." n n

    (erlang-skel-double-separator-start 3)
    "%%% API functions" n
    (erlang-skel-double-separator-end 3) n
    (erlang-skel-separator-start 2)
    "%% @doc" n
    "%% Starts the supervisor" n
    (erlang-skel-separator-end 2)
    "-spec start_link() -> {ok, Pid :: pid()} |" n>
    "{error, {already_started, Pid :: pid()}} |" n>
    "{error, {shutdown, term()}} |" n>
    "{error, term()} |" n>
    "ignore." n
    "start_link() ->" n>
    "supervisor:start_link({local, ?SERVER}, ?MODULE, [])." n
    n
    (erlang-skel-double-separator-start 3)
    "%%% Supervisor callbacks" n
    (erlang-skel-double-separator-end 3) n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% Whenever a supervisor is started using supervisor:start_link/[2,3]," n
    "%% this function is called by the new process to find out about" n
    "%% restart strategy, maximum restart intensity, and child" n
    "%% specifications." n
    (erlang-skel-separator-end 2)
    "-spec init(Args :: term()) ->" n>
    "{ok, {SupFlags :: supervisor:sup_flags()," n>
    "[ChildSpec :: supervisor:child_spec()]}} |" n>
    "ignore." n
    "init([]) ->" n
    "" n>
    "SupFlags = #{strategy => one_for_one," n>
    "intensity => 1," n>
    "period => 5}," n
    "" n>
    "AChild = #{id => 'AName'," n>
    "start => {'AModule', start_link, []}," n>
    "restart => permanent," n>
    "shutdown => 5000," n>
    "type => worker," n>
    "modules => ['AModule']}," n
    "" n>
    "{ok, {SupFlags, [AChild]}}." n
    n
    (erlang-skel-double-separator-start 3)
    "%%% Internal functions" n
    (erlang-skel-double-separator-end 3)
    )
  "*The template of a supervisor behaviour.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-supervisor-bridge
  '((erlang-skel-include erlang-skel-large-header)
    "-behaviour(supervisor_bridge)." n n

    "%% API" n
    "-export([start_link/0])." n n

    "%% supervisor_bridge callbacks" n
    "-export([init/1, terminate/2])." n n

    "-define(SERVER, ?MODULE)." n n

    "-record(state, {})." n n

    (erlang-skel-double-separator-start 3)
    "%%% API" n
    (erlang-skel-double-separator-end 3) n
    (erlang-skel-separator-start 2)
    "%% @doc" n
    "%% Starts the supervisor bridge" n
    (erlang-skel-separator-end 2)
    "-spec start_link() -> {ok, Pid :: pid()} |" n>
    "{error, {already_started, Pid :: pid()}} |" n>
    "{error, term()} |" n>
    "ignore." n
    "start_link() ->" n>
    "supervisor_bridge:start_link({local, ?SERVER}, ?MODULE, [])." n
    n
    (erlang-skel-double-separator-start 3)
    "%%% supervisor_bridge callbacks" n
    (erlang-skel-double-separator-end 3) n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% Creates a supervisor_bridge process, linked to the calling process," n
    "%% which calls Module:init/1 to start the subsystem. To ensure a" n
    "%% synchronized start-up procedure, this function does not return" n
    "%% until Module:init/1 has returned." n
    (erlang-skel-separator-end 2)
    "-spec init(Args :: term()) -> {ok, Pid :: pid(), State :: term()} |" n>
    "{error, Error :: term()} |" n>
    "ignore." n
    "init([]) ->" n>
    "case 'AModule':start_link() of" n>
    "{ok, Pid} ->" n>
    "{ok, Pid, #state{}};" n>
    "Error ->" n>
    "Error" n
    "end." > n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% This function is called by the supervisor_bridge when it is about" n
    "%% to terminate. It should be the opposite of Module:init/1 and stop" n
    "%% the subsystem and do any necessary cleaning up.The return value is" n
    "%% ignored." n
    (erlang-skel-separator-end 2)
    "-spec terminate(Reason :: shutdown | term(), State :: term()) -> any()." n
    "terminate(_Reason, _State) ->" n>
    "'AModule':stop()," n>
    "ok." n
    n
    (erlang-skel-double-separator-start 3)
    "%%% Internal functions" n
    (erlang-skel-double-separator-end 3)
    )
  "*The template of a supervisor_bridge behaviour.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-generic-server
  '((erlang-skel-include erlang-skel-large-header)
    "-behaviour(gen_server)." n n

    "%% API" n
    "-export([start_link/0])." n n

    "%% gen_server callbacks" n
    "-export([init/1, handle_call/3, handle_cast/2, handle_info/2," n>
    "terminate/2, code_change/3, format_status/2])." n n

    "-define(SERVER, ?MODULE)." n n

    "-record(state, {})." n n

    (erlang-skel-double-separator-start 3)
    "%%% API" n
    (erlang-skel-double-separator-end 3) n
    (erlang-skel-separator-start 2)
    "%% @doc" n
    "%% Starts the server" n
    (erlang-skel-separator-end 2)
    "-spec start_link() -> {ok, Pid :: pid()} |" n>
    "{error, Error :: {already_started, pid()}} |" n>
    "{error, Error :: term()} |" n>
    "ignore." n
    "start_link() ->" n>
    "gen_server:start_link({local, ?SERVER}, ?MODULE, [], [])." n
    n
    (erlang-skel-double-separator-start 3)
    "%%% gen_server callbacks" n
    (erlang-skel-double-separator-end 3)
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% Initializes the server" n
    (erlang-skel-separator-end 2)
    "-spec init(Args :: term()) -> {ok, State :: term()} |" n>
    "{ok, State :: term(), Timeout :: timeout()} |" n>
    "{ok, State :: term(), hibernate} |" n>
    "{stop, Reason :: term()} |" n>
    "ignore." n
    "init([]) ->" n>
    "process_flag(trap_exit, true)," n>
    "{ok, #state{}}." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% Handling call messages" n
    (erlang-skel-separator-end 2)
    "-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->" n>
    "{reply, Reply :: term(), NewState :: term()} |" n>
    "{reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |" n>
    "{reply, Reply :: term(), NewState :: term(), hibernate} |" n>
    "{noreply, NewState :: term()} |" n>
    "{noreply, NewState :: term(), Timeout :: timeout()} |" n>
    "{noreply, NewState :: term(), hibernate} |" n>
    "{stop, Reason :: term(), Reply :: term(), NewState :: term()} |" n>
    "{stop, Reason :: term(), NewState :: term()}." n
    "handle_call(_Request, _From, State) ->" n>
    "Reply = ok," n>
    "{reply, Reply, State}." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% Handling cast messages" n
    (erlang-skel-separator-end 2)
    "-spec handle_cast(Request :: term(), State :: term()) ->" n>
    "{noreply, NewState :: term()} |" n>
    "{noreply, NewState :: term(), Timeout :: timeout()} |" n>
    "{noreply, NewState :: term(), hibernate} |" n>
    "{stop, Reason :: term(), NewState :: term()}." n
    "handle_cast(_Request, State) ->" n>
    "{noreply, State}." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% Handling all non call/cast messages" n
    (erlang-skel-separator-end 2)
    "-spec handle_info(Info :: timeout() | term(), State :: term()) ->" n>
    "{noreply, NewState :: term()} |" n>
    "{noreply, NewState :: term(), Timeout :: timeout()} |" n>
    "{noreply, NewState :: term(), hibernate} |" n>
    "{stop, Reason :: normal | term(), NewState :: term()}." n
    "handle_info(_Info, State) ->" n>
    "{noreply, State}." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% This function is called by a gen_server when it is about to" n
    "%% terminate. It should be the opposite of Module:init/1 and do any" n
    "%% necessary cleaning up. When it returns, the gen_server terminates" n
    "%% with Reason. The return value is ignored." n
    (erlang-skel-separator-end 2)
    "-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term()," n>
    "State :: term()) -> any()." n
    "terminate(_Reason, _State) ->" n>
    "ok." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% Convert process state when code is changed" n
    (erlang-skel-separator-end 2)
    "-spec code_change(OldVsn :: term() | {down, term()}," n>
    "State :: term()," n>
    "Extra :: term()) -> {ok, NewState :: term()} |" n>
    "{error, Reason :: term()}." n
    "code_change(_OldVsn, State, _Extra) ->" n>
    "{ok, State}." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% This function is called for changing the form and appearance" n
    "%% of gen_server status when it is returned from sys:get_status/1,2" n
    "%% or when it appears in termination error logs." n
    (erlang-skel-separator-end 2)
    "-spec format_status(Opt :: normal | terminate," n>
    "Status :: list()) -> Status :: term()." n
    "format_status(_Opt, Status) ->" n>
    "Status." n
    n
    (erlang-skel-double-separator-start 3)
    "%%% Internal functions" n
    (erlang-skel-double-separator-end 3)
    )
  "*The template of a generic server.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-gen-event
  '((erlang-skel-include erlang-skel-large-header)
    "-behaviour(gen_event)." n n

    "%% API" n
    "-export([start_link/0, add_handler/0])." n n

    "%% gen_event callbacks" n
    "-export([init/1, handle_event/2, handle_call/2, handle_info/2," n>
    "terminate/2, code_change/3, format_status/2])." n n

    "-define(SERVER, ?MODULE)." n n

    "-record(state, {})." n n

    (erlang-skel-double-separator-start 3)
    "%%% API" n
    (erlang-skel-double-separator-end 3) n
    (erlang-skel-separator-start 2)
    "%% @doc" n
    "%% Creates an event manager" n
    (erlang-skel-separator-end 2)
    "-spec start_link() -> {ok, Pid :: pid()} |" n>
    "{error, Error :: {already_started, pid()} | term()}." n
    "start_link() ->" n>
    "gen_event:start_link({local, ?SERVER})." n
    n
    (erlang-skel-separator-start 2)
    "%% @doc" n
    "%% Adds an event handler" n
    (erlang-skel-separator-end 2)
    "-spec add_handler() -> ok | {'EXIT', Reason :: term()} | term()." n
    "add_handler() ->" n>
    "gen_event:add_handler(?SERVER, ?MODULE, [])." n
    n
    (erlang-skel-double-separator-start 3)
    "%%% gen_event callbacks" n
    (erlang-skel-double-separator-end 3) n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% Whenever a new event handler is added to an event manager," n
    "%% this function is called to initialize the event handler." n
    (erlang-skel-separator-end 2)
    "-spec init(Args :: term() | {Args :: term(), Term :: term()}) ->" n>
    "{ok, State :: term()} |" n>
    "{ok, State :: term(), hibernate} |" n>
    "{error, Reason :: term()}." n
    "init([]) ->" n>
    "{ok, #state{}}." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% Whenever an event manager receives an event sent using" n
    "%% gen_event:notify/2 or gen_event:sync_notify/2, this function is" n
    "%% called for each installed event handler to handle the event." n
    (erlang-skel-separator-end 2)
    "-spec handle_event(Event :: term(), State :: term()) ->" n>
    "{ok, NewState :: term()} |" n>
    "{ok, NewState :: term(), hibernate} |" n>
    "remove_handler |" n>
    "{swap_handler, Args1 :: term(), NewState :: term()," n>
    "Handler2 :: atom() | {atom(), term()} , Args2 :: term()}." n>
    "handle_event(_Event, State) ->" n>
    "{ok, State}." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% Whenever an event manager receives a request sent using" n
    "%% gen_event:call/3,4, this function is called for the specified" n
    "%% event handler to handle the request." n
    (erlang-skel-separator-end 2)
    "-spec handle_call(Request :: term(), State :: term()) ->" n>
    "{ok, Reply :: term(), NewState :: term()} |" n>
    "{ok, Reply :: term(), NewState :: term(), hibernate} |" n>
    "{remove_handler, Reply :: term()} |" n>
    "{swap_handler, Reply :: term(), Args1 :: term(), NewState :: term()," n>
    "Handler2 :: atom() | {atom(), term()}, Args2 :: term()}." n
    "handle_call(_Request, State) ->" n>
    "Reply = ok," n>
    "{ok, Reply, State}." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% This function is called for each installed event handler when" n
    "%% an event manager receives any other message than an event or a" n
    "%% synchronous request (or a system message)." n
    (erlang-skel-separator-end 2)
    "-spec handle_info(Info :: term(), State :: term()) ->" n>
    "{ok, NewState :: term()} |" n>
    "{ok, NewState :: term(), hibernate} |" n>
    "remove_handler |" n>
    "{swap_handler, Args1 :: term(), NewState :: term()," n>
    "Handler2 :: atom() | {atom(), term()}, Args2 :: term()}." n
    "handle_info(_Info, State) ->" n>
    "{ok, State}." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% Whenever an event handler is deleted from an event manager, this" n
    "%% function is called. It should be the opposite of Module:init/1 and" n
    "%% do any necessary cleaning up." n
    (erlang-skel-separator-end 2)
    "-spec terminate(Arg :: {stop, Reason :: term()} |" n>
    "stop |" n>
    "remove_handler |" n>
    "{error, {'EXIT', Reason :: term()}} |" n>
    "{error, Term :: term()} |" n>
    "term()," n>
    "State :: term()) -> any()." n
    "terminate(_Arg, _State) ->" n>
    "ok." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% Convert process state when code is changed" n
    (erlang-skel-separator-end 2)
    "-spec code_change(OldVsn :: term() | {down, term()}," n>
    "State :: term()," n>
    "Extra :: term()) -> {ok, NewState :: term()}." n
    "code_change(_OldVsn, State, _Extra) ->" n>
    "{ok, State}." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% This function is called for changing the form and appearance" n
    "%% of gen_event status when it is returned from sys:get_status/1,2" n
    "%% or when it appears in termination error logs." n
    (erlang-skel-separator-end 2)
    "-spec format_status(Opt :: normal | terminate," n>
    "Status :: list()) -> Status :: term()." n
    "format_status(_Opt, Status) ->" n>
    "Status." n
    n
    (erlang-skel-double-separator-start 3)
    "%%% Internal functions" n
    (erlang-skel-double-separator-end 3)
    )
  "*The template of a gen_event.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-gen-fsm
  '((erlang-skel-include erlang-skel-large-header)
    "-behaviour(gen_fsm)." n n

    "%% API" n
    "-export([start_link/0])." n n

    "%% gen_fsm callbacks" n
    "-export([init/1, state_name/2, state_name/3, handle_event/3," n>
    "handle_sync_event/4, handle_info/3, terminate/3, code_change/4])." n n

    "-define(SERVER, ?MODULE)." n n

    "-record(state, {})." n n

    (erlang-skel-double-separator-start 3)
    "%%% API" n
    (erlang-skel-double-separator-end 3) n
    (erlang-skel-separator-start 2)
    "%% @doc" n
    "%% Creates a gen_fsm process which calls Module:init/1 to" n
    "%% initialize. To ensure a synchronized start-up procedure, this" n
    "%% function does not return until Module:init/1 has returned." n
    "%%" n
    "%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}" n
    (erlang-skel-separator-end 2)
    "start_link() ->" n>
    "gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], [])." n
    n
    (erlang-skel-double-separator-start 3)
    "%%% gen_fsm callbacks" n
    (erlang-skel-double-separator-end 3) n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or" n
    "%% gen_fsm:start_link/[3,4], this function is called by the new" n
    "%% process to initialize." n
    "%%" n
    "%% @spec init(Args) -> {ok, StateName, State} |" n
    "%%                     {ok, StateName, State, Timeout} |" n
    "%%                     ignore |" n
    "%%                     {stop, StopReason}" n
    (erlang-skel-separator-end 2)
    "init([]) ->" n>
    "process_flag(trap_exit, true)," n>
    "{ok, state_name, #state{}}." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% There should be one instance of this function for each possible" n
    "%% state name. Whenever a gen_fsm receives an event sent using" n
    "%% gen_fsm:send_event/2, the instance of this function with the same" n
    "%% name as the current state name StateName is called to handle" n
    "%% the event. It is also called if a timeout occurs." n
    "%%" n
    "%% @spec state_name(Event, State) ->" n
    "%%                   {next_state, NextStateName, NextState} |" n
    "%%                   {next_state, NextStateName, NextState, Timeout} |" n
    "%%                   {stop, Reason, NewState}" n
    (erlang-skel-separator-end 2)
    "state_name(_Event, State) ->" n>
    "{next_state, state_name, State}." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% There should be one instance of this function for each possible" n
    "%% state name. Whenever a gen_fsm receives an event sent using" n
    "%% gen_fsm:sync_send_event/[2,3], the instance of this function with" n
    "%% the same name as the current state name StateName is called to" n
    "%% handle the event." n
    "%%" n
    "%% @spec state_name(Event, From, State) ->" n
    "%%                   {next_state, NextStateName, NextState} |"n
    "%%                   {next_state, NextStateName, NextState, Timeout} |" n
    "%%                   {reply, Reply, NextStateName, NextState} |" n
    "%%                   {reply, Reply, NextStateName, NextState, Timeout} |" n
    "%%                   {stop, Reason, NewState} |" n
    "%%                   {stop, Reason, Reply, NewState}" n
    (erlang-skel-separator-end 2)
    "state_name(_Event, _From, State) ->" n>
    "Reply = ok," n>
    "{reply, Reply, state_name, State}." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% Whenever a gen_fsm receives an event sent using" n
    "%% gen_fsm:send_all_state_event/2, this function is called to handle" n
    "%% the event." n
    "%%" n
    "%% @spec handle_event(Event, StateName, State) ->" n
    "%%                   {next_state, NextStateName, NextState} |" n
    "%%                   {next_state, NextStateName, NextState, Timeout} |" n
    "%%                   {stop, Reason, NewState}" n
    (erlang-skel-separator-end 2)
    "handle_event(_Event, StateName, State) ->" n>
    "{next_state, StateName, State}." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% Whenever a gen_fsm receives an event sent using" n
    "%% gen_fsm:sync_send_all_state_event/[2,3], this function is called" n
    "%% to handle the event." n
    "%%" n
    "%% @spec handle_sync_event(Event, From, StateName, State) ->" n
    "%%                   {next_state, NextStateName, NextState} |" n
    "%%                   {next_state, NextStateName, NextState, Timeout} |" n
    "%%                   {reply, Reply, NextStateName, NextState} |" n
    "%%                   {reply, Reply, NextStateName, NextState, Timeout} |" n
    "%%                   {stop, Reason, NewState} |" n
    "%%                   {stop, Reason, Reply, NewState}" n
    (erlang-skel-separator-end 2)
    "handle_sync_event(_Event, _From, StateName, State) ->" n>
    "Reply = ok," n>
    "{reply, Reply, StateName, State}." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% This function is called by a gen_fsm when it receives any" n
    "%% message other than a synchronous or asynchronous event" n
    "%% (or a system message)." n
    "%%" n
    "%% @spec handle_info(Info,StateName,State)->" n
    "%%                   {next_state, NextStateName, NextState} |" n
    "%%                   {next_state, NextStateName, NextState, Timeout} |" n
    "%%                   {stop, Reason, NewState}" n
    (erlang-skel-separator-end 2)
    "handle_info(_Info, StateName, State) ->" n>
    "{next_state, StateName, State}." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% This function is called by a gen_fsm when it is about to" n
    "%% terminate. It should be the opposite of Module:init/1 and do any" n
    "%% necessary cleaning up. When it returns, the gen_fsm terminates with" n
    "%% Reason. The return value is ignored." n
    "%%" n
    "%% @spec terminate(Reason, StateName, State) -> void()" n
    (erlang-skel-separator-end 2)
    "terminate(_Reason, _StateName, _State) ->" n>
    "ok." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% Convert process state when code is changed" n
    "%%" n
    "%% @spec code_change(OldVsn, StateName, State, Extra) ->" n
    "%%                   {ok, StateName, NewState}" n
    (erlang-skel-separator-end 2)
    "code_change(_OldVsn, StateName, State, _Extra) ->" n>
    "{ok, StateName, State}." n
    n
    (erlang-skel-double-separator-start 3)
    "%%% Internal functions" n
    (erlang-skel-double-separator-end 3)
    )
  "*The template of a gen_fsm.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-gen-statem-StateName
  '((erlang-skel-include erlang-skel-large-header)
    "-behaviour(gen_statem)." n n

    "%% API" n
    "-export([start_link/0])." n
    n
    "%% gen_statem callbacks" n
    "-export([callback_mode/0, init/1, terminate/3, code_change/4])." n
    "-export([state_name/3])." n
    n
    "-define(SERVER, ?MODULE)." n
    n
    "-record(data, {})." n
    n
    (erlang-skel-double-separator-start 3)
    "%%% API" n
    (erlang-skel-double-separator-end 3) n
    (erlang-skel-separator-start 2)
    "%% @doc" n
    "%% Creates a gen_statem process which calls Module:init/1 to" n
    "%% initialize. To ensure a synchronized start-up procedure, this" n
    "%% function does not return until Module:init/1 has returned." n
    "%%" n
    (erlang-skel-separator-end 2)
    "-spec start_link() ->" n>
    "{ok, Pid :: pid()} |" n>
    "ignore |" n>
    "{error, Error :: term()}." n
    "start_link() ->" n>
    "gen_statem:start_link({local, ?SERVER}, ?MODULE, [], [])." n
    n
    (erlang-skel-double-separator-start 3)
    "%%% gen_statem callbacks" n
    (erlang-skel-double-separator-end 3) n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% Define the callback_mode() for this callback module." n
    (erlang-skel-separator-end 2)
    "-spec callback_mode() -> gen_statem:callback_mode_result()." n
    "callback_mode() -> state_functions." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% Whenever a gen_statem is started using gen_statem:start/[3,4] or" n
    "%% gen_statem:start_link/[3,4], this function is called by the new" n
    "%% process to initialize." n
    (erlang-skel-separator-end 2)
    "-spec init(Args :: term()) ->" n>
    "gen_statem:init_result(atom())." n
    "init([]) ->" n>
    "process_flag(trap_exit, true)," n>
    "{ok, state_name, #data{}}." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% There should be one function like this for each state name." n
    "%% Whenever a gen_statem receives an event, the function " n
    "%% with the name of the current state (StateName) " n
    "%% is called to handle the event." n
    (erlang-skel-separator-end 2)
    "-spec state_name('enter'," n>
    "OldState :: atom()," n>
    "Data :: term()) ->" n>
    "gen_statem:state_enter_result('state_name');" n>
    "(gen_statem:event_type()," n>
    "Msg :: term()," n>
    "Data :: term()) ->" n>
    "gen_statem:event_handler_result(atom())." n
    ;;
    "state_name({call,Caller}, _Msg, Data) ->" n>
    "{next_state, state_name, Data, [{reply,Caller,ok}]}." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% This function is called by a gen_statem when it is about to" n
    "%% terminate. It should be the opposite of Module:init/1 and do any" n
    "%% necessary cleaning up. When it returns, the gen_statem terminates with" n
    "%% Reason. The return value is ignored." n
    (erlang-skel-separator-end 2)
    "-spec terminate(Reason :: term(), State :: term(), Data :: term()) ->" n>
    "any()." n
    "terminate(_Reason, _State, _Data) ->" n>
    "void." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% Convert process state when code is changed" n
    (erlang-skel-separator-end 2)
    "-spec code_change(" n>
    "OldVsn :: term() | {down,term()}," n>
    "State :: term(), Data :: term(), Extra :: term()) ->" n>
    "{ok, NewState :: term(), NewData :: term()} |" n>
    "(Reason :: term())." n
    "code_change(_OldVsn, State, Data, _Extra) ->" n>
    "{ok, State, Data}." n
    n
    (erlang-skel-double-separator-start 3)
    "%%% Internal functions" n
    (erlang-skel-double-separator-end 3)
    )
  "*The template of a gen_statem (StateName/3).
Please see the function `tempo-define-template'.")

(defvar erlang-skel-gen-statem-handle-event
  '((erlang-skel-include erlang-skel-large-header)
    "-behaviour(gen_statem)." n n

    "%% API" n
    "-export([start_link/0])." n
    n
    "%% gen_statem callbacks" n
    "-export([callback_mode/0, init/1, terminate/3, code_change/4])." n
    "-export([handle_event/4])." n
    n
    "-define(SERVER, ?MODULE)." n
    n
    "-record(data, {})." n
    n
    (erlang-skel-double-separator-start 3)
    "%%% API" n
    (erlang-skel-double-separator-end 3) n
    (erlang-skel-separator-start 2)
    "%% @doc" n
    "%% Creates a gen_statem process which calls Module:init/1 to" n
    "%% initialize. To ensure a synchronized start-up procedure, this" n
    "%% function does not return until Module:init/1 has returned." n
    "%%" n
    (erlang-skel-separator-end 2)
    "-spec start_link() ->" n>
    "{ok, Pid :: pid()} |" n>
    "ignore |" n>
    "{error, Error :: term()}." n
    "start_link() ->" n>
    "gen_statem:start_link({local, ?SERVER}, ?MODULE, [], [])." n
    n
    (erlang-skel-double-separator-start 3)
    "%%% gen_statem callbacks" n
    (erlang-skel-double-separator-end 3) n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% Define the callback_mode() for this callback module." n
    (erlang-skel-separator-end 2)
    "-spec callback_mode() -> gen_statem:callback_mode_result()." n
    "callback_mode() -> handle_event_function." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% Whenever a gen_statem is started using gen_statem:start/[3,4] or" n
    "%% gen_statem:start_link/[3,4], this function is called by the new" n
    "%% process to initialize." n
    (erlang-skel-separator-end 2)
    "-spec init(Args :: term()) ->" n>
    "gen_statem:init_result(term())." n
    "init([]) ->" n>
    "process_flag(trap_exit, true)," n>
    "{ok, state_name, #data{}}." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% This function is called for every event a gen_statem receives." n
    (erlang-skel-separator-end 2)
    "-spec handle_event('enter'," n>
    "OldState :: term()," n>
    "State :: term()," n>
    "Data :: term()) ->" n>
    "gen_statem:state_enter_result(term());" n>
    "(gen_statem:event_type()," n>
    "Msg :: term()," n>
    "State :: term()," n>
    "Data :: term()) ->" n>
    "gen_statem:event_handler_result(term())." n
    ;;
    "handle_event({call,From}, _Msg, State, Data) ->" n>
    "{next_state, State, Data, [{reply,From,ok}]}." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% This function is called by a gen_statem when it is about to" n
    "%% terminate. It should be the opposite of Module:init/1 and do any" n
    "%% necessary cleaning up. When it returns, the gen_statem terminates with" n
    "%% Reason. The return value is ignored." n
    (erlang-skel-separator-end 2)
    "-spec terminate(Reason :: term(), State :: term(), Data :: term()) ->" n>
    "any()." n
    "terminate(_Reason, _State, _Data) ->" n>
    "void." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% Convert process state when code is changed" n
    (erlang-skel-separator-end 2)
    "-spec code_change(" n>
    "OldVsn :: term() | {down,term()}," n>
    "State :: term(), Data :: term(), Extra :: term()) ->" n>
    "{ok, NewState :: term(), NewData :: term()} |" n>
    "(Reason :: term())." n
    "code_change(_OldVsn, State, Data, _Extra) ->" n>
    "{ok, State, Data}." n
    n
    (erlang-skel-double-separator-start 3)
    "%%% Internal functions" n
    (erlang-skel-double-separator-end 3)
    )
  "*The template of a gen_statem (handle_event/4).
Please see the function `tempo-define-template'.")

(defvar erlang-skel-wx-object
  '((erlang-skel-include erlang-skel-large-header)
    "-behaviour(wx_object)." n n

    "-include_lib(\"wx/include/wx.hrl\")." n n

    "%% API" n
    "-export([start_link/0])." n n

    "%% wx_object callbacks" n
    "-export([init/1, handle_call/3, handle_cast/2, "
    "handle_info/2," n>
    "handle_event/2, terminate/2, code_change/3])." n n

    "-record(state, {})." n n

    (erlang-skel-double-separator-start 3)
    "%%% API" n
    (erlang-skel-double-separator-end 3) n
    (erlang-skel-separator-start 2)
    "%% @doc" n
    "%% Starts the server" n
    "%%" n
    "%% @spec start_link() -> wxWindow()" n
    (erlang-skel-separator-end 2)
    "start_link() ->" n>
    "wx_object:start_link(?MODULE, [], [])." n
    n
    (erlang-skel-double-separator-start 3)
    "%%% wx_object callbacks" n
    (erlang-skel-double-separator-end 3)
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% Initializes the server" n
    "%%" n
    "%% @spec init(Args) -> {wxWindow(), State} |" n
    "%%                     {wxWindow(), State, Timeout} |" n
    "%%                     ignore |" n
    "%%                     {stop, Reason}" n
    (erlang-skel-separator-end 2)
    "init([]) ->" n>
    "wx:new()," n>
    "Frame = wxFrame:new()," n>
    "{Frame, #state{}}." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% Handling events" n
    "%%" n
    "%% @spec handle_event(wx{}, State) ->" n
    "%%                                   {noreply, State} |" n
    "%%                                   {noreply, State, Timeout} |" n
    "%%                                   {stop, Reason, State}" n
    (erlang-skel-separator-end 2)
    "handle_event(#wx{}, State) ->" n>
    "{noreply, State}." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% Handling call messages" n
    "%%" n
    "%% @spec handle_call(Request, From, State) ->" n
    "%%                                   {reply, Reply, State} |" n
    "%%                                   {reply, Reply, State, Timeout} |" n
    "%%                                   {noreply, State} |" n
    "%%                                   {noreply, State, Timeout} |" n
    "%%                                   {stop, Reason, Reply, State} |" n
    "%%                                   {stop, Reason, State}" n
    (erlang-skel-separator-end 2)
    "handle_call(_Request, _From, State) ->" n>
    "Reply = ok," n>
    "{reply, Reply, State}." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% Handling cast messages" n
    "%%" n
    "%% @spec handle_cast(Msg, State) -> {noreply, State} |" n
    "%%                                  {noreply, State, Timeout} |" n
    "%%                                  {stop, Reason, State}" n
    (erlang-skel-separator-end 2)
    "handle_cast(_Msg, State) ->" n>
    "{noreply, State}." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% Handling all non call/cast messages" n
    "%%" n
    "%% @spec handle_info(Info, State) -> {noreply, State} |" n
    "%%                                   {noreply, State, Timeout} |" n
    "%%                                   {stop, Reason, State}" n
    (erlang-skel-separator-end 2)
    "handle_info(_Info, State) ->" n>
    "{noreply, State}." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% This function is called by a wx_object when it is about to" n
    "%% terminate. It should be the opposite of Module:init/1 and do any" n
    "%% necessary cleaning up. When it returns, the wx_object terminates" n
    "%% with Reason. The return value is ignored." n
    "%%" n
    "%% @spec terminate(Reason, State) -> void()" n
    (erlang-skel-separator-end 2)
    "terminate(_Reason, _State) ->" n>
    "ok." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% Convert process state when code is changed" n
    "%%" n
    "%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}" n
    (erlang-skel-separator-end 2)
    "code_change(_OldVsn, State, _Extra) ->" n>
    "{ok, State}." n
    n
    (erlang-skel-double-separator-start 3)
    "%%% Internal functions" n
    (erlang-skel-double-separator-end 3)
    )
  "*The template of a generic server.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-lib
  '((erlang-skel-include erlang-skel-large-header)

    "%% API" n
    "-export([])." n n

    (erlang-skel-double-separator-start 3)
    "%%% API" n
    (erlang-skel-double-separator-end 3) n
    (erlang-skel-separator-start 2)
    "%% @doc" n
    "%% @spec" n
    (erlang-skel-separator-end 2)
    n
    (erlang-skel-double-separator-start 3)
    "%%% Internal functions" n
    (erlang-skel-double-separator-end 3)
    )
  "*The template of a library module.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-corba-callback
  '((erlang-skel-include erlang-skel-large-header)
    "%% Include files" n n

    "%% API" n
    "-export([])." n n

    "%% Corba callbacks" n
    "-export([init/1, terminate/2, code_change/3])." n n

    "-record(state, {})." n n

    (erlang-skel-double-separator-start 3)
    "%%% Corba callbacks" n
    (erlang-skel-double-separator-end 3) n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% Initializes the server" n
    "%%" n
    "%% @spec init(Args) -> {ok, State} |" n
    "%%                     {ok, State, Timeout} |" n
    "%%                     ignore |" n
    "%%                     {stop, Reason}" n
    (erlang-skel-separator-end 2)
    "init([]) ->" n>
    "{ok, #state{}}." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% Shutdown the server" n
    "%%" n
    "%% @spec terminate(Reason, State) -> void()" n
    (erlang-skel-separator-end 2)
    "terminate(_Reason, _State) ->" n>
    "ok." n
    n
    (erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc" n
    "%% Convert process state when code is changed" n
    "%%" n
    "%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}" n
    (erlang-skel-separator-end 2)
    "code_change(_OldVsn, State, _Extra) ->" n>
    "{ok, State}." n
    n
    (erlang-skel-double-separator-start 3)
    "%%% Internal functions" n
    (erlang-skel-double-separator-end 3)
    )
  "*The template of a library module.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-ts-test-suite
 '((erlang-skel-include erlang-skel-large-header)
   "%% Note: This directive should only be used in test suites." n
    "-compile(export_all)." n n

    "-include_lib(\"common_test/include/ct.hrl\")." n n

    (erlang-skel-separator-start 2)
    "%% TEST SERVER CALLBACK FUNCTIONS" n
    (erlang-skel-separator 2)
    n
    (erlang-skel-separator-start 2)
    "%%" n
    "%% @doc" n
    "%% Initialization before the suite." n
    "%%" n
    "%% Config0 = Config1 = [tuple()]" n
    "%%   A list of key/value pairs, holding the test case configuration." n
    "%% Reason = term()" n
    "%%   The reason for skipping the suite." n
    "%%" n
    "%% Note: This function is free to add any key/value pairs to the Config" n
    "%% variable, but should NOT alter/remove any existing entries." n
    "%%" n
    "%% @spec init_per_suite(Config) -> Config" n
    (erlang-skel-separator-end 2)
    "init_per_suite(Config) ->" n >
    "Config." n n

    (erlang-skel-separator-start 2)
    "%% @doc" n
    "%% Cleanup after the suite." n
    "%% Config - [tuple()]" n
    "%%   A list of key/value pairs, holding the test case configuration." n
    "%%" n
    "%% @spec end_per_suite(Config) -> _" n
    (erlang-skel-separator-end 2)
    "end_per_suite(_Config) ->" n >
    "ok." n n

    (erlang-skel-separator-start 2)
    "%% @doc" n
    "%% Initialization before each test case" n
    "%%" n
    "%% TestCase - atom()" n
    "%%   Name of the test case that is about to be run." n
    "%% Config - [tuple()]" n
    "%%   A list of key/value pairs, holding the test case configuration." n
    "%% Reason = term()" n
    "%%   The reason for skipping the test case." n
    "%%" n
    "%% Note: This function is free to add any key/value pairs to the Config" n
    "%% variable, but should NOT alter/remove any existing entries." n
    "%%" n
    "%% @spec init_per_testcase(TestCase, Config) -> Config" n
    (erlang-skel-separator-end 2)
    "init_per_testcase(_TestCase, Config) ->" n >
    "Config." n n

    (erlang-skel-separator-start 2)
    "%% @doc" n
    "%% Cleanup after each test case" n
    "%%" n
    "%% TestCase = atom()" n
    "%%   Name of the test case that is finished." n
    "%% Config = [tuple()]" n
    "%%   A list of key/value pairs, holding the test case configuration." n
    "%%" n
    "%% @spec end_per_testcase(TestCase, Config) -> _" n
    (erlang-skel-separator-end 2)
    "end_per_testcase(_TestCase, _Config) ->" n >
    "ok."n n

    (erlang-skel-separator-start 2)
    "%% @doc" n
    "%% Returns a description of the test suite when" n
    "%% Clause == doc, and a test specification (list" n
    "%% of the conf and test cases in the suite) when" n
    "%% Clause == suite." n   
    "%% Returns a list of all test cases in this test suite" n
    "%%" n
    "%% Clause = doc | suite" n
    "%%   Indicates expected return value." n
    "%% Descr = [string()] | []" n
    "%%   String that describes the test suite." n
    "%% Spec = [TestCase]" n
    "%%   A test specification." n
    "%% TestCase = ConfCase | atom()" n
    "%%   Configuration case, or the name of a test case function." n
    "%% ConfCase = {conf,Init,Spec,End} |" n
    "%%            {conf,Properties,Init,Spec,End}" n
    "%% Init = End = {Mod,Func} | Func" n
    "%%   Initialization and cleanup function." n
    "%% Mod = Func = atom()" n
    "%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]" n
    "%%   Execution properties of the test cases (may be combined)." n
    "%% Shuffle = shuffle | {shuffle,Seed}" n
    "%%   To get cases executed in random order." n
    "%% Seed = {integer(),integer(),integer()}" n
    "%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |" n
    "%%              repeat_until_any_ok | repeat_until_any_fail" n
    "%%   To get execution of cases repeated." n
    "%% N = integer() | forever" n
    "%% Reason = term()" n
    "%%   The reason for skipping the test suite." n
    "%%" n
    "%% @spec all(Clause) -> TestCases" n
    (erlang-skel-separator-end 2)
    "all(doc) ->" n >
    "[\"Describe the main purpose of this suite\"];" n n
    "all(suite) -> " n >
    "[a_test_case]." n n
    n
    (erlang-skel-separator-start 2)
    "%% TEST CASES" n
    (erlang-skel-separator 2)
    n
    (erlang-skel-separator-start 2)
    "%% @doc" n
    "%%  Test case function. Returns a description of the test" n
    "%%  case (doc), then returns a test specification (suite)," n
    "%%  or performs the actual test (Config)." n
    "%%" n
    "%% Arg = doc | suite | Config" n
    "%%   Indicates expected behaviour and return value." n
    "%% Config = [tuple()]" n
    "%%   A list of key/value pairs, holding the test case configuration." n
    "%% Descr = [string()] | []" n
    "%%   String that describes the test case." n
    "%% Spec = [tuple()] | []" n
    "%%   A test specification, see all/1." n
    "%% Reason = term()" n
    "%%   The reason for skipping the test case." n
    "%%" n
    "%% @spec TestCase(Arg) -> Descr | Spec | ok | exit() | {skip,Reason}" n

    (erlang-skel-separator-end 2)
    "a_test_case(doc) -> " n >
    "[\"Describe the main purpose of this test case\"];" n n
    "a_test_case(suite) -> " n >
    "[];" n n
    "a_test_case(Config) when is_list(Config) -> " n >
    "ok." n
   )
 "*The template of a library module.
Please see the function `tempo-define-template'.")

(defvar erlang-skel-ct-test-suite-s
  '((erlang-skel-include erlang-skel-large-header)
    "-compile(export_all)." n n

    "-include_lib(\"common_test/include/ct.hrl\")." n n

    (erlang-skel-separator-start 2)
    "%% @spec suite() -> Info" n
    "%% Info = [tuple()]" n
    (erlang-skel-separator-end 2)
    "suite() ->" n >
    "[{timetrap,{seconds,30}}]." n n

    (erlang-skel-separator-start 2)
    "%% @spec init_per_suite(Config0) ->" n
    "%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}" n
    "%% Config0 = Config1 = [tuple()]" n
    "%% Reason = term()" n
    (erlang-skel-separator-end 2)
    "init_per_suite(Config) ->" n >
    "Config." n n

    (erlang-skel-separator-start 2)
    "%% @spec end_per_suite(Config0) -> term() | {save_config,Config1}" n
    "%% Config0 = Config1 = [tuple()]" n
    (erlang-skel-separator-end 2)
    "end_per_suite(_Config) ->" n >
    "ok." n n

    (erlang-skel-separator-start 2)
    "%% @spec init_per_group(GroupName, Config0) ->" n
    "%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}" n
    "%% GroupName = atom()" n
    "%% Config0 = Config1 = [tuple()]" n
    "%% Reason = term()" n
    (erlang-skel-separator-end 2)
    "init_per_group(_GroupName, Config) ->" n >
    "Config." n n

    (erlang-skel-separator-start 2)
    "%% @spec end_per_group(GroupName, Config0) ->" n
    "%%               term() | {save_config,Config1}" n
    "%% GroupName = atom()" n
    "%% Config0 = Config1 = [tuple()]" n
    (erlang-skel-separator-end 2)
    "end_per_group(_GroupName, _Config) ->" n >
    "ok." n n

    (erlang-skel-separator-start 2)
    "%% @spec init_per_testcase(TestCase, Config0) ->" n
    "%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}" n
    "%% TestCase = atom()" n
    "%% Config0 = Config1 = [tuple()]" n
    "%% Reason = term()" n
    (erlang-skel-separator-end 2)
    "init_per_testcase(_TestCase, Config) ->" n >
    "Config." n n

    (erlang-skel-separator-start 2)
    "%% @spec end_per_testcase(TestCase, Config0) ->" n
    "%%               term() | {save_config,Config1} | {fail,Reason}" n
    "%% TestCase = atom()" n
    "%% Config0 = Config1 = [tuple()]" n
    "%% Reason = term()" n
    (erlang-skel-separator-end 2)
    "end_per_testcase(_TestCase, _Config) ->" n >
    "ok." n n

    (erlang-skel-separator-start 2)
    "%% @spec groups() -> [Group]" n
    "%% Group = {GroupName,Properties,GroupsAndTestCases}" n
    "%% GroupName = atom()" n
    "%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]" n
    "%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]" n
    "%% TestCase = atom()" n
    "%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}" n
    "%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |" n
    "%%              repeat_until_any_ok | repeat_until_any_fail" n
    "%% N = integer() | forever" n
    (erlang-skel-separator-end 2)
    "groups() ->" n >
    "[]." n n

    (erlang-skel-separator-start 2)
    "%% @spec all() -> GroupsAndTestCases | {skip,Reason}" n
    "%% GroupsAndTestCases = [{group,GroupName} | TestCase]" n
    "%% GroupName = atom()" n
    "%% TestCase = atom()" n
    "%% Reason = term()" n
    (erlang-skel-separator-end 2)
    "all() -> " n >
    "[my_test_case]." n n

    (erlang-skel-separator-start 2)
    "%% @spec TestCase() -> Info" n
    "%% Info = [tuple()]" n
    (erlang-skel-separator-end 2)
    "my_test_case() -> " n >
    "[]." n n

    (erlang-skel-separator-start 2)
    "%% @spec TestCase(Config0) ->" n
    "%%               ok | exit() | {skip,Reason} | {comment,Comment} |" n
    "%%               {save_config,Config1} | {skip_and_save,Reason,Config1}" n
    "%% Config0 = Config1 = [tuple()]" n
    "%% Reason = term()" n
    "%% Comment = term()" n
    (erlang-skel-separator-end 2)
    "my_test_case(_Config) -> " n >
    "ok." n
    )
  "*The template of a library module.
Please see the function `tempo-define-template'.")


(defvar erlang-skel-ct-test-suite-l
  '((erlang-skel-include erlang-skel-large-header)
    "%% Note: This directive should only be used in test suites." n
    "-compile(export_all)." n n

    "-include_lib(\"common_test/include/ct.hrl\")." n n

    (erlang-skel-separator-start 2)
    "%% COMMON TEST CALLBACK FUNCTIONS" n
    (erlang-skel-separator 2)
    n
    (erlang-skel-separator-start 2)
    "%% @doc" n
    "%%  Returns list of tuples to set default properties" n
    "%%  for the suite." n
    "%%" n
    "%% Function: suite() -> Info" n
    "%%" n
    "%% Info = [tuple()]" n
    "%%   List of key/value pairs." n
    "%%" n
    "%% Note: The suite/0 function is only meant to be used to return" n
    "%% default data values, not perform any other operations." n
    "%%" n
    "%% @spec suite() -> Info" n
    (erlang-skel-separator-end 2)
    "suite() ->" n >
    "[{timetrap,{minutes,10}}]." n n

    (erlang-skel-separator-start 2)
    "%% @doc" n    
    "%% Initialization before the whole suite" n
    "%%" n
    "%% Config0 = Config1 = [tuple()]" n
    "%%   A list of key/value pairs, holding the test case configuration." n
    "%% Reason = term()" n
    "%%   The reason for skipping the suite." n
    "%%" n
    "%% Note: This function is free to add any key/value pairs to the Config" n
    "%% variable, but should NOT alter/remove any existing entries." n
    "%%" n
    "%% @spec init_per_suite(Config0) ->" n
    "%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}" n
    (erlang-skel-separator-end 2)
    "init_per_suite(Config) ->" n >
    "Config." n n

    (erlang-skel-separator-start 2)
    "%% @doc" n
    "%% Cleanup after the whole suite" n
    "%%" n
    "%% Config - [tuple()]" n
    "%%   A list of key/value pairs, holding the test case configuration." n
    "%%" n
    "%% @spec end_per_suite(Config) -> _" n
    (erlang-skel-separator-end 2)
    "end_per_suite(_Config) ->" n >
    "ok." n n

    (erlang-skel-separator-start 2)
    "%% @doc" n
    "%% Initialization before each test case group." n
    "%%" n
    "%% GroupName = atom()" n
    "%%   Name of the test case group that is about to run." n
    "%% Config0 = Config1 = [tuple()]" n
    "%%   A list of key/value pairs, holding configuration data for the group." n
    "%% Reason = term()" n
    "%%   The reason for skipping all test cases and subgroups in the group." n
    "%%" n
    "%% @spec init_per_group(GroupName, Config0) ->" n
    "%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}" n
    (erlang-skel-separator-end 2)
    "init_per_group(_GroupName, Config) ->" n >
    "Config." n n

    (erlang-skel-separator-start 2)
    "%% @doc" n
    "%% Cleanup after each test case group." n
    "%%" n
    "%% GroupName = atom()" n
    "%%   Name of the test case group that is finished." n
    "%% Config0 = Config1 = [tuple()]" n
    "%%   A list of key/value pairs, holding configuration data for the group." n
    "%%" n
    "%% @spec end_per_group(GroupName, Config0) ->" n
    "%%               term() | {save_config,Config1}" n
    (erlang-skel-separator-end 2)
    "end_per_group(_GroupName, _Config) ->" n >
    "ok." n n
    (erlang-skel-separator-start 2)
    "%% @doc" n
    "%% Initialization before each test case" n
    "%%" n
    "%% TestCase - atom()" n
    "%%   Name of the test case that is about to be run." n
    "%% Config0 = Config1 = [tuple()]" n
    "%%   A list of key/value pairs, holding the test case configuration." n
    "%% Reason = term()" n
    "%%   The reason for skipping the test case." n
    "%%" n
    "%% Note: This function is free to add any key/value pairs to the Config" n
    "%% variable, but should NOT alter/remove any existing entries." n
    "%%" n
    "%% @spec init_per_testcase(TestCase, Config0) ->" n
    "%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}" n
    (erlang-skel-separator-end 2)
    "init_per_testcase(_TestCase, Config) ->" n >
    "Config." n n

    (erlang-skel-separator-start 2)
    "%% @doc" n
    "%% Cleanup after each test case" n
    "%%" n
    "%% TestCase - atom()" n
    "%%   Name of the test case that is finished." n
    "%% Config0 = Config1 = [tuple()]" n
    "%%   A list of key/value pairs, holding the test case configuration." n
    "%%" n
    "%% @spec end_per_testcase(TestCase, Config0) ->" n
    "%%               term() | {save_config,Config1} | {fail,Reason}" n
    (erlang-skel-separator-end 2)
    "end_per_testcase(_TestCase, _Config) ->" n >
    "ok." n n

    (erlang-skel-separator-start 2)
    "%% @doc" n
    "%% Returns a list of test case group definitions." n
    "%%" n
    "%% Group = {GroupName,Properties,GroupsAndTestCases}" n
    "%% GroupName = atom()" n
    "%%   The name of the group." n
    "%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]" n
    "%%   Group properties that may be combined." n
    "%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]" n
    "%% TestCase = atom()" n
    "%%   The name of a test case." n
    "%% Shuffle = shuffle | {shuffle,Seed}" n
    "%%   To get cases executed in random order." n
    "%% Seed = {integer(),integer(),integer()}" n
    "%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |" n
    "%%              repeat_until_any_ok | repeat_until_any_fail" n
    "%%   To get execution of cases repeated." n
    "%% N = integer() | forever" n
    "%%" n
    "%% @spec: groups() -> [Group]" n
    (erlang-skel-separator-end 2)
    "groups() ->" n >
    "[]." n n

    (erlang-skel-separator-start 2)
    "%% @doc" n 
    "%%  Returns the list of groups and test cases that" n
    "%%  are to be executed." n
    "%%" n
    "%% GroupsAndTestCases = [{group,GroupName} | TestCase]" n
    "%% GroupName = atom()" n
    "%%   Name of a test case group." n
    "%% TestCase = atom()" n
    "%%   Name of a test case." n
    "%% Reason = term()" n
    "%%   The reason for skipping all groups and test cases." n
    "%%" n
    "%% @spec all() -> GroupsAndTestCases | {skip,Reason}" n
    (erlang-skel-separator-end 2)
    "all() -> " n >
    "[my_test_case]." n n

    n
    (erlang-skel-separator-start 2)
    "%% TEST CASES" n
    (erlang-skel-separator 2)
    n

    (erlang-skel-separator-start 2)
    "%% @doc " n
    "%%  Test case info function - returns list of tuples to set" n
    "%%  properties for the test case." n
    "%%" n
    "%% Info = [tuple()]" n
    "%%   List of key/value pairs." n
    "%%" n
    "%% Note: This function is only meant to be used to return a list of" n
    "%% values, not perform any other operations." n
    "%%" n
    "%% @spec TestCase() -> Info " n
    (erlang-skel-separator-end 2)
    "my_test_case() -> " n >
    "[]." n n

    (erlang-skel-separator 2)
    "%% @doc Test case function. (The name of it must be specified in" n
    "%%              the all/0 list or in a test case group for the test case" n
    "%%              to be executed)." n
    "%%" n
    "%% Config0 = Config1 = [tuple()]" n
    "%%   A list of key/value pairs, holding the test case configuration." n
    "%% Reason = term()" n
    "%%   The reason for skipping the test case." n
    "%% Comment = term()" n
    "%%   A comment about the test case that will be printed in the html log." n
    "%%" n
    "%% @spec TestCase(Config0) ->" n
    "%%           ok | exit() | {skip,Reason} | {comment,Comment} |" n
    "%%           {save_config,Config1} | {skip_and_save,Reason,Config1}" n
    (erlang-skel-separator-end 2)
    "my_test_case(_Config) -> " n >
    "ok." n

    )
 "*The template of a library module.
 Please see the function `tempo-define-template'.")

(defvar erlang-skel-ct-hook
  '((erlang-skel-include erlang-skel-normal-header)
    "%% Mandatory callbacks" n
    "-export([init/2])." n
    n
    "%% Optional callbacks" n
    "-export([id/1])." n
    n
    "-export([pre_init_per_suite/3])." n
    "-export([post_init_per_suite/4])." n
    n
    "-export([pre_end_per_suite/3])." n
    "-export([post_end_per_suite/4])." n
    n
    "-export([pre_init_per_group/4])." n
    "-export([post_init_per_group/5])." n
    n
    "-export([pre_end_per_group/4])." n
    "-export([post_end_per_group/5])." n
    n
    "-export([pre_init_per_testcase/4])." n
    "-export([post_init_per_testcase/5])." n
    n
    "-export([pre_end_per_testcase/4])." n
    "-export([post_end_per_testcase/5])." n
    n
    "-export([on_tc_fail/4])." n
    "-export([on_tc_skip/4])." n
    n
    "-export([terminate/1])." n
    n
    "%% The hook state is threaded through all callbacks," n
    "%% but can be anything the hook needs, replace as desired." n
    "-record(state, { cases=0, suites=0, groups=0, skips=0, fails=0 })." n
    n
    "%% Return a unique id for this CTH." n
    "id(Opts) ->" n
    "    %% A reference is the default implementation, this can be removed" n
    "    %% or some value can be read from Opts instead." n
    "    erlang:make_ref()." n
    n
    "%% Always called before any other callback function, once per installed hook." n
    "init(Id, Opts) ->" n
    "    {ok, #state{}}." n
    n
    "pre_init_per_suite(Suite, Config, State) ->" n
    "    {Config, State}." n
    n
    "post_init_per_suite(Suite, Config, Return, State) ->" n
    "    {Return, State}." n
    n
    "pre_end_per_suite(Suite, Config, State) ->" n
    "    {Config, State}." n
    n
    "post_end_per_suite(Suite, Config, Return, State) ->" n
    "    {Return, State#state{suites = State#state.suites + 1}}." n
    n
    "pre_init_per_group(Suite, Group, Config, State) ->" n
    "    {Config, State}." n
    n
    "post_init_per_group(Suite, Group, Config, Return, State) ->" n
    "    {Return, State}." n
    n
    "pre_end_per_group(Suite, Group, Config, State) ->" n
    "    {Config, State}." n
    n
    "post_end_per_group(Suite, Group, Config, Return, State) ->" n
    "    {Return, State#state{groups = State#state.groups + 1}}." n
    n
    "pre_init_per_testcase(Suite, TC, Config, State) ->" n
    "    {Config, State}." n
    n
    "%% Called after each init_per_testcase (immediately before the test case)." n
    "post_init_per_testcase(Suite, TC, Config, Return, State) ->" n
    "    {Return, State}." n
    n
    "%% Called before each end_per_testcase (immediately after the test case)." n
    "pre_end_per_testcase(Suite, TC, Config, State) ->" n
    "    {Config, State}." n
    n
    "post_end_per_testcase(Suite, TC, Config, Return, State) ->" n
    "    {Return, State#state{cases = State#state.cases + 1}}." n
    n
    "%% Called after post_init_per_suite, post_end_per_suite, post_init_per_group," n
    "%% post_end_per_group and post_end_per_testcase if the suite, group or test case failed." n
    "on_tc_fail(Suite, TC, Reason, State) ->" n
    "    State#state{fails = State#state.fails + 1}." n
    n
    "%% Called when a test case is skipped by either user action" n
    "%% or due to an init function failing." n
    "on_tc_skip(Suite, TC, Reason, State) ->" n
    "    State#state{skips = State#state.skips + 1}." n
    n
    "%% Called when the scope of the CTH is done" n
    "terminate(State) ->" n
    "    logger:notice(\"~s is done: ~p~n\", [?MODULE, State])." n
    n
    )
 "*The template of a library module.
 Please see the function `tempo-define-template'.")

;; Skeleton code:

;; This code is based on the package `tempo' which is part of modern
;; Emacsen.  (GNU Emacs 19.25 (?) and XEmacs 19.14.)

(defun erlang-skel-init ()
  "Generate the skeleton functions and menu items.
The variable `erlang-skel' contains the name and descriptions of
all skeletons.

The skeleton routines are based on the `tempo' package.  Should this
package not be present, this function does nothing."
  (interactive)
  (condition-case nil
      (require 'tempo)
    (error t))
  (if (featurep 'tempo)
      (let ((skel erlang-skel)
            (menu '()))
        (while skel
          (cond ((null (car skel))
                 (setq menu (cons nil menu)))
                (t
                 (funcall (symbol-function 'tempo-define-template)
                          (concat "erlang-" (nth 1 (car skel)))
                          ;; The tempo template used contains an `include'
                          ;; function call only, hence changes to the
                          ;; variables describing the templates take effect
                          ;; immdiately.
                          (list (list 'erlang-skel-include (nth 2 (car skel))))
                          (nth 1 (car skel)))
                 (setq menu (cons (erlang-skel-make-menu-item
                                   (car skel)) menu))))
          (setq skel (cdr skel)))
        (setq erlang-menu-skel-items
              (list nil (list "Skeletons" (nreverse menu))))
        (setq erlang-menu-items
              (erlang-menu-add-above 'erlang-menu-skel-items
                                     'erlang-menu-version-items
                                     erlang-menu-items))
        (erlang-menu-init))))

(defun erlang-skel-make-menu-item (skel)
  (let ((func (intern (concat "tempo-template-erlang-" (nth 1 skel)))))
    (cond ((null (nth 3 skel))
           (list (car skel) func))
          (t
           (list (car skel)
                 (list 'lambda '()
                       '(interactive)
                       (list 'funcall
                             (list 'quote (nth 3 skel))
                             (list 'quote func))))))))

;; Functions designed to be added to the skeleton menu.
;; (Not normally used)
(defun erlang-skel-insert (func)
  "Insert skeleton generated by FUNC and goto first tempo mark."
  (save-excursion (funcall func))
  (funcall (symbol-function 'tempo-forward-mark)))

(defun erlang-skel-header (func)
  "Insert the header generated by FUNC at the beginning of the buffer."
  (goto-char (point-min))
  (save-excursion (funcall func))
  (funcall (symbol-function 'tempo-forward-mark)))


;; Functions used inside the skeleton descriptions.
(defun erlang-skel-skip-blank ()
  (skip-chars-backward " \t")
  nil)

(defun erlang-skel-include (&rest args)
  "Include a template inside another template.

Example of use, assuming that `erlang-skel-func' is defined:

 (defvar foo-skeleton \\='(\"%%% New function:\"
                        (erlang-skel-include erlang-skel-func)))

Technically, this function returns the `tempo' attribute`(l ...)' which
can contain other `tempo' attributes.  Please see the function
`tempo-define-template' for a description of the `(l ...)' attribute."
  (let ((res '())
        entry)
    (while args
      (setq entry (car args))
      (while entry
        (setq res (cons (car entry) res))
        (setq entry (cdr entry)))
      (setq args (cdr args)))
    (cons 'l (nreverse res))))

(defun erlang-skel-separator (&optional percent)
  "Return a comment separator."
  (let ((percent (or percent 3)))
    (concat (make-string percent ?%)
            (make-string (- 70 percent) ?-)
            "\n")))

(defun erlang-skel-separator-start (&optional percent)
  "Return a comment separator or an empty string if separators
are configured off."
  (if erlang-skel-use-separators
      (erlang-skel-separator percent)
    ""))

(defun erlang-skel-separator-end (&optional percent)
  "Return a comment separator to end a function comment block or an
empty string if separators are configured off."
  (if erlang-skel-use-separators
      (concat "%% @end\n" (erlang-skel-separator percent))
    ""))

(defun erlang-skel-double-separator (&optional percent)
  "Return a double line (equals sign) comment separator."
  (let ((percent (or percent 3)))
    (concat (make-string percent ?%)
            (make-string (- 70 percent) ?=)
            "\n")))

(defun erlang-skel-double-separator-start (&optional percent)
  "Return a double separator or a newline if separators are configured off."
  (if erlang-skel-use-separators
      (erlang-skel-double-separator percent)
    "\n"))

(defun erlang-skel-double-separator-end (&optional percent)
  "Return a double separator or an empty string if separators are
configured off."
  (if erlang-skel-use-separators
      (erlang-skel-double-separator percent)
    ""))

(defun erlang-skel-dd-mmm-yyyy ()
  "Return the current date as a string in \"DD Mon YYYY\" form.
The first character of DD is space if the value is less than 10."
  (let ((date (current-time-string)))
    (format "%2d %s %s"
            (string-to-number (substring date 8 10))
            (substring date 4 7)
            (substring date -4))))

(defun erlang-skel-get-function-name ()
  (save-excursion
    (erlang-beginning-of-function -1)
    (erlang-get-function-name)))

(defun erlang-skel-get-function-args ()
  (save-excursion
    (erlang-beginning-of-function -1)
    (erlang-get-function-arguments)))

;; Local variables:
;; coding: iso-8859-1
;; End:

;;; erlang-skels.el ends here
