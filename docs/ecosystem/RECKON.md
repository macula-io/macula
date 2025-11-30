# Reckon Architecture

> **Audience:** Developers, Technical Architects
> **Last Updated:** 2025-11-28

Event sourcing and CQRS patterns for building Macula applications.

---

## What is Reckon?

Reckon is an architectural pattern library that defines conventions for building event-sourced systems on the BEAM. It provides the structural foundation that makes Macula applications consistent, maintainable, and scalable.

**Reckon is not a library you install** - it's a set of patterns you follow when building Macula applications.

---

## Core Principles

### 1. Vertical Slicing

Each business operation is a self-contained "slice" with all its components co-located:

```
my_app/
+-- initialize_account/
|   +-- command.ex       # Input structure
|   +-- handler.ex       # Business logic
|   +-- event.ex         # Output (what happened)
|   +-- initialized_to_summary.ex  # Projection
|
+-- activate_account/
|   +-- command.ex
|   +-- handler.ex
|   +-- event.ex
|   +-- activated_to_summary.ex
|
+-- close_account/
    +-- command.ex
    +-- handler.ex
    +-- event.ex
    +-- closed_to_summary.ex
```

**Benefits:**
- Changes to one operation don't affect others
- Related code is co-located (easy to find)
- Each slice testable independently
- Business operations visible from folder structure

### 2. Screaming Architecture

The codebase "screams" its business intent:

```
# Bad: Technical layers hide business intent
lib/
+-- commands/
+-- handlers/
+-- events/
+-- projections/

# Good: Business operations are obvious
lib/
+-- join_queue/
+-- start_game/
+-- score_point/
+-- end_match/
```

### 3. Events are Facts

Events are immutable records of what happened - not CRUD operations:

```elixir
# Bad: Technical/CRUD events
AccountCreated
AccountUpdated
AccountDeleted

# Good: Business-meaningful events
AccountInitialized
AccountActivated
AccountSuspendedForNonPayment
AccountClosedByOwner
```

### 4. Projections Transform Events to Read Models

Projections listen to events and build optimized read models:

```
Event Stream                 Projection              Read Model
+----------------+          +----------+          +-------------+
| AccountInitialized | ---> |          |          |             |
| AccountActivated   | ---> | Summary  | -------> | Account     |
| DepositMade        | ---> | Projector|          | Summary     |
| WithdrawalMade     | ---> |          |          | (one row)   |
+----------------+          +----------+          +-------------+

- Events: Full history
- Read Model: Current state, optimized for queries
```

---

## Pattern Details

### Commands

Commands represent intent - a request to do something:

```elixir
defmodule MyApp.JoinQueue.Command do
  @enforce_keys [:player_id, :queue_type]
  defstruct [:player_id, :queue_type, :skill_rating]

  @type t :: %__MODULE__{
    player_id: String.t(),
    queue_type: :ranked | :casual,
    skill_rating: integer() | nil
  }

  def new(attrs) do
    struct!(__MODULE__, attrs)
  end

  def validate(%__MODULE__{} = cmd) do
    with :ok <- validate_player_id(cmd.player_id),
         :ok <- validate_queue_type(cmd.queue_type) do
      {:ok, cmd}
    end
  end
end
```

**Naming:** Imperative present tense - `join_queue`, `start_game`, `score_point`

### Events

Events record what happened - immutable facts:

```elixir
defmodule MyApp.JoinQueue.Event do
  @derive Jason.Encoder
  defstruct [:player_id, :queue_type, :skill_rating, :joined_at]

  @type t :: %__MODULE__{
    player_id: String.t(),
    queue_type: :ranked | :casual,
    skill_rating: integer(),
    joined_at: DateTime.t()
  }
end
```

**Naming:** Past tense - `player_queued`, `game_started`, `point_scored`

### Handlers

Handlers contain business logic that processes commands and emits events:

```elixir
defmodule MyApp.JoinQueue.Handler do
  alias MyApp.JoinQueue.{Command, Event}

  def handle(%Command{} = cmd, %{} = state) do
    with :ok <- validate_not_already_queued(cmd.player_id, state),
         :ok <- validate_queue_open(cmd.queue_type) do
      event = %Event{
        player_id: cmd.player_id,
        queue_type: cmd.queue_type,
        skill_rating: cmd.skill_rating || calculate_default_rating(),
        joined_at: DateTime.utc_now()
      }
      {:ok, event}
    end
  end
end
```

### Projections

Projections transform events into read models:

```elixir
defmodule MyApp.JoinQueue.QueuedToSummary do
  use Commanded.Projections.Ecto, repo: MyApp.Repo

  project(%MyApp.JoinQueue.Event{} = event, _metadata, fn multi ->
    summary = %QueueSummary{
      player_id: event.player_id,
      queue_type: event.queue_type,
      skill_rating: event.skill_rating,
      status: :waiting,
      queued_at: event.joined_at
    }

    Ecto.Multi.insert(
      multi,
      :queue_summary,
      summary,
      on_conflict: :replace_all,
      conflict_target: [:player_id]
    )
  end)
end
```

**Key Rules:**
- Projections must be idempotent (safe to replay)
- No complex business logic in projections
- Only use data from the event itself
- Name pattern: `[event]_to_[read_model].ex`

---

## Macula Integration

### Topic Design

Macula pub/sub topics follow Reckon conventions:

```elixir
# Bad: Entity ID in topic (causes topic explosion)
"game.#{game_id}.state"
"player.#{player_id}.matched"

# Good: ID in payload (scalable)
"game.state"        # Payload: %{game_id: "...", ...}
"player.matched"    # Payload: %{player_id: "...", ...}
```

### RPC Design

RPC procedure names follow command naming:

```elixir
# Commands become RPC procedures
join_queue     -> "matchmaking.join_queue"
start_game     -> "game.start_game"
score_point    -> "game.score_point"

# Not technical names
"matchmaking.create"     # Bad: CRUD-style
"game.update"            # Bad: Technical
```

### Event Distribution

Events can be published to the mesh for distributed systems:

```elixir
defmodule MyApp.JoinQueue.Publisher do
  def after_dispatch(%JoinQueue.Event{} = event) do
    macula:publish(
      peer(),
      <<"matchmaking.player_queued">>,
      encode(event)
    )
  end
end
```

Other nodes subscribe to events they care about:

```elixir
macula:subscribe(peer(), <<"matchmaking.player_queued">>, fn event ->
  # Update local read model or trigger local logic
  update_matchmaking_view(event)
end)
```

---

## Anti-Patterns

### Mixing Technical and Business Concerns

```elixir
# Bad: Generic modules with multiple operations
defmodule MyApp.Commands do
  defmodule CreateAccount do ... end
  defmodule UpdateAccount do ... end
end

# Good: One module per operation
defmodule MyApp.InitializeAccount.Command do ... end
defmodule MyApp.ActivateAccount.Command do ... end
```

### Non-Idempotent Projections

```elixir
# Bad: Fails on replay
Ecto.Multi.insert(:summary, %Summary{...})

# Good: Safe to replay
Ecto.Multi.insert(:summary, %Summary{...},
  on_conflict: :nothing,
  conflict_target: [:id]
)
```

### Business Logic in Projections

```elixir
# Bad: Complex logic in projection
project(event, _meta, fn multi ->
  tier = calculate_subscription_tier(event)  # Business logic!
  pricing = fetch_pricing(tier)               # External call!
  ...
end)

# Good: Only data transformation
project(event, _meta, fn multi ->
  summary = %Summary{
    id: event.account_id,
    status: event.status,
    updated_at: event.occurred_at
  }
  ...
end)
```

---

## Testing

Tests follow the same vertical structure:

```
test/
+-- my_app/
    +-- join_queue/
    |   +-- command_test.exs
    |   +-- handler_test.exs
    |   +-- event_test.exs
    |   +-- queued_to_summary_test.exs
    +-- start_game/
        +-- command_test.exs
        +-- handler_test.exs
        +-- event_test.exs
```

**Example projection test:**

```elixir
defmodule MyApp.JoinQueue.QueuedToSummaryTest do
  use MyApp.DataCase

  alias MyApp.JoinQueue.{Event, QueuedToSummary}
  alias MyApp.QueueSummary

  test "projects player_queued to queue_summary" do
    event = %Event{
      player_id: "player-123",
      queue_type: :ranked,
      skill_rating: 1500,
      joined_at: ~U[2025-01-15 10:00:00Z]
    }

    :ok = QueuedToSummary.handle(event, %{})

    summary = Repo.get!(QueueSummary, "player-123")
    assert summary.queue_type == :ranked
    assert summary.skill_rating == 1500
    assert summary.status == :waiting
  end
end
```

---

## See Also

- [Developer Guide](../developer/) - Building with Macula
- [Ecosystem Overview](README.md)
- [reckon_docs repository](https://github.com/reckon-db-org/reckon_docs)

