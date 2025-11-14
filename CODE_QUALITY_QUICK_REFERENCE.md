     STDIN
   1 # Code Quality Improvements: Quick Reference Guide
   2 
   3 **Last Updated:** November 14, 2025  
   4 **Full Analysis:** See `CODE_QUALITY_ANALYSIS_PHASE2.md` (1,007 lines)
   5 
   6 ---
   7 
   8 ## Health Score Summary
   9 
  10 **Current:** 6.8/10  
  11 **Target:** 8.0+/10  
  12 **Estimated Effort:** 35-45 hours across phases
  13 
  14 ---
  15 
  16 ## Top 10 High-Impact Refactorings
  17 
  18 ### 1. Extract Message Type Classifier (HIGHEST IMPACT)
  19 - **File:** `src/macula_routing_server.erl:271-287`
  20 - **Issue:** 3 levels of nesting in DHT message dispatch (hot path)
  21 - **Refactoring:** Extract `classify_message/1` function
  22 - **Effort:** 1-2 hours
  23 - **Impact:** HIGH - Fixes critical hot path, enables easy extension
  24 
  25 ```erlang
  26 % Before: 3 nested case statements
  27 process_dht_message(Message, State) ->
  28     case is_find_node(Message) of
  29         true -> handle_find_node(...);
  30         false -> case is_store(Message) of
  31             true -> handle_store(...);
  32             false -> case is_find_value(Message) of
  33                 ...
  34             end
  35         end
  36     end.
  37 
  38 % After: Single level nesting + pure classifier
  39 process_dht_message(Message, State) ->
  40     dispatch_dht_message(classify_message(Message), Message, State).
  41 
  42 classify_message(Message) ->
  43     case {is_find_node(Message), is_store(Message), is_find_value(Message)} of
  44         {true, _, _} -> find_node;
  45         {_, true, _} -> store;
  46         {_, _, true} -> find_value;
  47         _ -> unknown
  48     end.
  49 ```
  50 
  51 ---
  52 
  53 ### 2. Simplify Provider List Updates
  54 - **File:** `src/macula_routing_server.erl:124-157`
  55 - **Issue:** 3 nested case statements for provider management
  56 - **Refactoring:** Extract `ensure_provider_list/1`, `update_provider_list/2`
  57 - **Effort:** 2-3 hours
  58 - **Impact:** HIGH - DHT storage operation used frequently
  59 
  60 ```erlang
  61 % Extract these helper functions with guards
  62 ensure_provider_list(Providers) when is_list(Providers) -> Providers;
  63 ensure_provider_list(Provider) -> [Provider].
  64 
  65 update_provider_list(ProviderList, NewProvider) ->
  66     NodeId = maps:get(node_id, NewProvider, undefined),
  67     replace_or_append(NodeId, NewProvider, ProviderList).
  68 
  69 replace_or_append(undefined, Provider, ProviderList) ->
  70     [Provider | ProviderList];
  71 replace_or_append(NodeId, Provider, ProviderList) ->
  72     case find_provider_index(NodeId, ProviderList) of
  73         not_found -> [Provider | ProviderList];
  74         Index -> update_provider_at_index(Index, Provider, ProviderList)
  75     end.
  76 ```
  77 
  78 ---
  79 
  80 ### 3. Fix Service Registry Guard Consistency
  81 - **File:** `src/macula_service_registry.erl:234-250`
  82 - **Issue:** Using `if` inside fold (mixes imperative styles)
  83 - **Refactoring:** Extract `filter_expired_entry/5` with guard-based dispatch
  84 - **Effort:** 1 hour
  85 - **Impact:** MEDIUM - Core discovery module, consistency improvement
  86 
  87 ```erlang
  88 % Replace if-statement with guards
  89 filter_by_age(true, _ServiceId, _CacheEntry, {Acc, Count}) ->
  90     {Acc, Count + 1};  % Expired - skip
  91 filter_by_age(false, ServiceId, CacheEntry, {Acc, Count}) ->
  92     {Acc#{ServiceId => CacheEntry}, Count}.  % Keep entry
  93 ```
  94 
  95 ---
  96 
  97 ### 4. Simplify RPC Handler Dispatch
  98 - **File:** `src/macula_rpc_handler.erl:117-160`
  99 - **Issue:** Nested case for service discovery with 2+ levels of nesting
 100 - **Refactoring:** Extract `handle_call_dispatch/6`, `route_rpc_call/6`
 101 - **Effort:** 3-4 hours
 102 - **Impact:** MEDIUM - Core RPC operations
 103 
 104 ```erlang
 105 handle_call({call, Procedure, Args, Opts}, From, State) ->
 106     Registry = State#state.service_registry,
 107     BinaryProcedure = ensure_binary(Procedure),
 108     handle_call_dispatch(
 109         macula_service_registry:get_local_handler(Registry, BinaryProcedure),
 110         BinaryProcedure, Args, Opts, From, State
 111     ).
 112 
 113 % Pattern: local handler exists
 114 handle_call_dispatch({ok, Handler}, _Proc, Args, _Opts, From, State) ->
 115     spawn(fun() -> execute_local_handler(Handler, Args, From) end),
 116     {noreply, State};
 117 
 118 % Pattern: need service discovery
 119 handle_call_dispatch(not_found, Procedure, Args, Opts, From, State) ->
 120     Registry = State#state.service_registry,
 121     route_rpc_call(Procedure, Args, Opts, From, State, Registry).
 122 ```
 123 
 124 ---
 125 
 126 ### 5. Extract Pub/Sub Subscription Logic
 127 - **File:** `src/macula_gateway_pubsub.erl:92-150`
 128 - **Issue:** 2+ levels of nesting in subscribe handler, hard to read state updates
 129 - **Refactoring:** Extract `is_already_subscribed/3`, `add_subscription/3`, `maybe_add_monitor/4`
 130 - **Effort:** 2-3 hours
 131 - **Impact:** MEDIUM - PubSub operations
 132 
 133 ```erlang
 134 handle_call({subscribe, Stream, Topic}, _From, State) ->
 135     NewState = handle_subscribe_request(Stream, Topic, State),
 136     {reply, ok, NewState}.
 137 
 138 handle_subscribe_request(Stream, Topic, State) ->
 139     case is_already_subscribed(Stream, Topic, State) of
 140         true -> State;
 141         false -> add_subscription(Stream, Topic, State)
 142     end.
 143 
 144 is_already_subscribed(Stream, Topic, State) ->
 145     CurrentTopics = maps:get(Stream, State#state.stream_subscriptions, []),
 146     lists:member(Topic, CurrentTopics).
 147 ```
 148 
 149 ---
 150 
 151 ### 6. Standardize ensure_binary Conversion
 152 - **File:** Create central utility in `src/macula_utils.erl`
 153 - **Issue:** DRY - function defined in multiple modules
 154 - **Refactoring:** Create `to_binary/1` utility with pattern matching
 155 - **Effort:** 1 hour
 156 - **Impact:** LOW - Consistency and maintainability
 157 
 158 ```erlang
 159 -spec to_binary(term()) -> binary().
 160 to_binary(Data) when is_binary(Data) -> Data;
 161 to_binary(Data) when is_list(Data) -> iolist_to_binary(Data);
 162 to_binary(Data) when is_atom(Data) -> atom_to_binary(Data);
 163 to_binary(Data) -> term_to_binary(Data).
 164 ```
 165 
 166 ---
 167 
 168 ### 7. Extract Common Message Patterns
 169 - **Files:** Multiple gateway modules
 170 - **Issue:** Repeated code for building messages, encoding payloads
 171 - **Refactoring:** Create `macula_message_builder.erl` module
 172 - **Effort:** 2-3 hours
 173 - **Impact:** MEDIUM - Reduces duplication across modules
 174 
 175 ---
 176 
 177 ### 8. Standardize Connection Liveness Checks
 178 - **Files:** `src/macula_gateway_mesh.erl`, `src/macula_connection_manager.erl`
 179 - **Issue:** Duplicated connection validation logic
 180 - **Refactoring:** Create `macula_connection_utils.erl` with reusable checks
 181 - **Effort:** 1 hour
 182 - **Impact:** LOW - Code consistency
 183 
 184 ---
 185 
 186 ### 9. Improve Error Handling Consistency
 187 - **Issue:** Some modules use overly broad `catch _:_`
 188 - **Refactoring:** Use specific error patterns where possible
 189 - **Effort:** 2-3 hours
 190 - **Impact:** MEDIUM - Easier debugging and error tracking
 191 
 192 ```erlang
 193 % Prefer specific catches
 194 catch Error:Reason when Error =:= function_clause -> handle_error(Reason)
 195 % Over broad catches
 196 catch _:_ -> default_handler()
 197 ```
 198 
 199 ---
 200 
 201 ### 10. Add Comprehensive Tests for Extracted Functions
 202 - **Target Modules:** All newly extracted functions
 203 - **Effort:** 8+ hours
 204 - **Impact:** HIGH - Ensures refactoring safety and prevents regressions
 205 
 206 ---
 207 
 208 ## Refactoring Phases
 209 
 210 ### Phase 1: High-Impact, Low-Effort (1-2 weeks)
 211 **Goal:** Fix hot paths and improve clarity
 212 
 213 - [ ] Extract message type classifier (macula_routing_server) - 1-2h
 214 - [ ] Simplify DHT message dispatch (macula_routing_server) - 2h
 215 - [ ] Consolidate ensure_binary utilities - 1h
 216 - [ ] Fix service registry guards (macula_service_registry) - 1h
 217 - [ ] Simplify RPC handler dispatch (macula_rpc_handler) - 3-4h
 218 - [ ] Add tests for refactored code - 5-8h
 219 
 220 **Subtotal:** 13-19 hours  
 221 **Health Score Target:** 6.8 → 7.2-7.4
 222 
 223 ---
 224 
 225 ### Phase 2: Medium-Impact, Medium-Effort (2-3 weeks)
 226 **Goal:** Improve handler function clarity and test coverage
 227 
 228 - [ ] Extract pubsub subscription logic (macula_gateway_pubsub) - 2-3h
 229 - [ ] Extract connection setup helpers (macula_connection_manager) - 2h
 230 - [ ] Simplify provider list updates (macula_routing_server) - 2-3h
 231 - [ ] Extract common message patterns - 2-3h
 232 - [ ] Standardize utility functions - 1-2h
 233 - [ ] Add comprehensive tests - 8-10h
 234 
 235 **Subtotal:** 17-23 hours  
 236 **Health Score Target:** 7.2 → 7.8-8.0
 237 
 238 ---
 239 
 240 ### Phase 3: Pattern Cleanup & Documentation (3-4 weeks)
 241 **Goal:** Full consistency and improved maintainability
 242 
 243 - [ ] Refactor remaining case statements in handlers
 244 - [ ] Improve error handling consistency
 245 - [ ] Document idiomatic patterns for team
 246 - [ ] Add performance optimization tests
 247 - [ ] Create style guide for future development
 248 
 249 **Subtotal:** 15-20 hours  
 250 **Health Score Target:** 7.8 → 8.2+
 251 
 252 ---
 253 
 254 ## Refactoring Checklist
 255 
 256 When implementing each refactoring:
 257 
 258 - [ ] **Read the full section** in `CODE_QUALITY_ANALYSIS_PHASE2.md`
 259 - [ ] **Understand existing behavior** before changing
 260 - [ ] **Create feature branch** for the refactoring
 261 - [ ] **Extract functions** following shown patterns
 262 - [ ] **Write tests first** (if TDD approach) or alongside changes
 263 - [ ] **Verify behavior preserved** with existing tests
 264 - [ ] **Update documentation** if specs/docs change
 265 - [ ] **Get code review** before merging
 266 - [ ] **Measure impact** on health score metrics
 267 
 268 ---
 269 
 270 ## Tools & Commands
 271 
 272 ### Analyze Case Statements
 273 ```bash
 274 grep -r "case " src/*.erl | wc -l
 275 ```
 276 
 277 ### Find Deep Nesting
 278 ```bash
 279 grep -r "^\s\{12,\}" src/*.erl
 280 ```
 281 
 282 ### Check Type Specs
 283 ```bash
 284 grep -c "^-spec" src/*.erl
 285 ```
 286 
 287 ### Run Tests
 288 ```bash
 289 rebar3 eunit
 290 ```
 291 
 292 ---
 293 
 294 ## Key Principles to Remember
 295 
 296 1. **Pattern Matching Over Case Statements**
 297    - Use function clauses with guards instead of case when possible
 298    - Makes code more declarative and easier to read
 299 
 300 2. **Guards > If Statements**
 301    - Prefer guards for type checking and simple conditions
 302    - If statements are less idiomatic in Erlang
 303 
 304 3. **Keep Nesting ≤ 2 Levels**
 305    - Extract helper functions to reduce nesting depth
 306    - Each function should do one thing clearly
 307 
 308 4. **DRY Principle**
 309    - Extract repeated patterns to utility modules
 310    - Makes code easier to maintain and test
 311 
 312 5. **Test Coverage First**
 313    - Add tests when extracting functions
 314    - Ensures behavior is preserved during refactoring
 315 
 316 ---
 317 
 318 ## Questions & Issues
 319 
 320 For detailed analysis of specific sections, see:
 321 - **Case Statements:** Part 1 in `CODE_QUALITY_ANALYSIS_PHASE2.md`
 322 - **Deep Nesting:** Part 2 in `CODE_QUALITY_ANALYSIS_PHASE2.md`
 323 - **Error Handling:** Part 4 in `CODE_QUALITY_ANALYSIS_PHASE2.md`
 324 - **Code Duplication:** Part 5 in `CODE_QUALITY_ANALYSIS_PHASE2.md`
 325 
 326 ---
 327 
 328 ## Next Steps
 329 
 330 1. **Review this quick reference** with the team
 331 2. **Read full analysis** in `CODE_QUALITY_ANALYSIS_PHASE2.md`
 332 3. **Pick Phase 1 item** to start with
 333 4. **Create feature branch** and implement following shown patterns
 334 5. **Add tests** to ensure behavior is preserved
 335 6. **Get code review** before merging
 336 7. **Celebrate improvement** to health score!
 337 
