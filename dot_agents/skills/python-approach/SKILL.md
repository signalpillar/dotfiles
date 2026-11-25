---
name: python-approach
description: Standard architecture and engineering approach for Python scripts, CLI tools, and services: centralized filesystem layout (Layout pattern), unified environment variable management (Env pattern) with field-level defaults, strict Pydantic data modeling, making illegal states unrepresentable via discriminated unions (Jane Street principle), and dependency injection via Injector.
---

# Codebase Architecture: Layout, Env, Domain Modeling & Dependency Injection

A disciplined architectural standard for modern Python codebases:
1. **Never scatter filesystem path joins or environment variable lookups across application logic.**
2. **Model all domain data classes using strict, immutable Pydantic models.**
3. **Make illegal states unrepresentable** (the Jane Street principle): replace bags of optional fields and boolean flags with algebraic discriminated unions.
4. **Write injectable code**: decouple business logic from object creation using the `injector` library.

---

## 1. The Layout Pattern (`layout.py`)

### Core Principles
- **One Single Source of Truth for Paths**: Every filesystem path, directory, artifact, log file, database file, and cache location is computed in `layout.py`.
- **Field-Level Defaults**: Directory and file names are declared as field-level defaults on the layout class (`workspace_dirname: str = Field(default="workspace")`), eliminating magic string literals.
- **Pure & Deterministic**: Path computation models are frozen (`ConfigDict(frozen=True)`). Given the same root or input, they always produce the same `Path` object without performing I/O or reading global state.
- **Zero Ad-Hoc String Joins**: Application logic, routes, and services query properties on layout instances (e.g. `layout.workspace_dir`, `layout.notes_file`). Segment literals appear nowhere else in the codebase.
- **Clear Separation of Concerns**: Callers perform I/O *against* layout paths, but layout classes never perform disk operations themselves.

### Canonical Structure

```python
"""Every path in the project, computed in one place.

Pydantic model instances, passed around and queried through properties.
All directory and file name defaults are declared at the field level.
Callers perform I/O against layout paths but never join segments themselves.
"""

from __future__ import annotations

from pathlib import Path
from typing import TYPE_CHECKING
from pydantic import BaseModel, ConfigDict, Field

if TYPE_CHECKING:
    from env import Env


class ProjectLayout(BaseModel):
    """Paths rooted at the project root directory with field-level defaults."""

    model_config = ConfigDict(frozen=True, arbitrary_types_allowed=True)

    root: Path

    # Directory name defaults declared on the field level
    workspace_dirname: str = Field(default="workspace")
    memories_dirname: str = Field(default="memories")
    logs_dirname: str = Field(default="logs")

    # File name defaults declared on the field level
    soul_filename: str = Field(default="soul.md")
    notes_filename: str = Field(default="notes.jsonl")
    sessions_db_filename: str = Field(default="sessions.db")
    agent_log_filename: str = Field(default="agent.log")

    # Optional custom directory path overrides (from Env)
    custom_workspace: Path | None = None
    custom_memories: Path | None = None
    custom_logs: Path | None = None
    custom_sessions_db: Path | None = None

    @classmethod
    def discover(cls, start: Path | None = None) -> ProjectLayout:
        """Discover project root from current file or provided path."""
        base = start or Path(__file__).resolve().parent
        return cls(root=base)

    @classmethod
    def from_env(cls, env: Env, start: Path | None = None) -> ProjectLayout:
        """Construct layout applying environment directory overrides."""
        base = start or Path(__file__).resolve().parent
        return cls(
            root=base,
            custom_workspace=env.workspace_dir,
            custom_memories=env.memories_dir,
            custom_logs=env.log_dir,
            custom_sessions_db=env.sessions_db_file,
        )

    # Computed path properties
    @property
    def workspace_dir(self) -> Path:
        return self.custom_workspace or (self.root / self.workspace_dirname)

    @property
    def soul_file(self) -> Path:
        return self.workspace_dir / self.soul_filename

    @property
    def memories_dir(self) -> Path:
        return self.custom_memories or (self.root / self.memories_dirname)

    @property
    def notes_file(self) -> Path:
        return self.memories_dir / self.notes_filename

    @property
    def sessions_db_file(self) -> Path:
        return self.custom_sessions_db or (self.memories_dir / self.sessions_db_filename)

    @property
    def logs_dir(self) -> Path:
        return self.custom_logs or (self.root / self.logs_dirname)

    @property
    def agent_log_file(self) -> Path:
        return self.logs_dir / self.agent_log_filename
```

---

## 2. The Env Pattern (`env.py` / `config.py`)

### Core Principles
- **Sole Reader of Process Environment**: `env.py` is the **ONLY** module in the entire project permitted to call `os.environ`, `os.getenv`, or read `.env`.
- **Field-Level Defaults**: All configuration keys are declared with static types, explicit defaults defined directly on the field level (`Field(default=...)`), and aliases via `pydantic-settings` (`BaseSettings`).
- **Fail Early**: Missing required variables or malformed values raise descriptive errors at startup, rather than deep in a running pipeline.
- **Provider via Dependency Injection**: Rather than importing a mutable global singleton across 20 files, inject `Env` through the dependency injection container.

### Canonical Structure

```python
"""Application settings and environment configuration.

This is the ONLY module that reads process environment variables.
All defaults are declared at the field level on the Env class.
"""

from __future__ import annotations

from pathlib import Path
from pydantic import AliasChoices, Field
from pydantic_settings import BaseSettings, SettingsConfigDict


class Env(BaseSettings):
    """Every tunable of the application with types, validation, and field-level defaults."""

    model_config = SettingsConfigDict(
        env_file=".env",
        env_file_encoding="utf-8",
        extra="ignore",
    )

    # Core runtime settings with field-level defaults
    llm_provider: str = Field(
        default="antigravity",
        validation_alias=AliasChoices("llm_provider", "LLM_PROVIDER"),
    )
    llm_model: str | None = Field(
        default=None,
        validation_alias=AliasChoices("llm_model", "LLM_MODEL"),
    )

    # Provider models & keys
    anthropic_api_key: str | None = Field(
        default=None,
        validation_alias=AliasChoices("anthropic_api_key", "ANTHROPIC_API_KEY"),
    )
    gemini_api_key: str | None = Field(
        default=None,
        validation_alias=AliasChoices("gemini_api_key", "GEMINI_API_KEY"),
    )

    # Networking
    rtc_port_min: int = Field(
        default=50000,
        validation_alias=AliasChoices("rtc_port_min", "RTC_PORT_MIN"),
    )
    rtc_port_max: int = Field(
        default=50020,
        validation_alias=AliasChoices("rtc_port_max", "RTC_PORT_MAX"),
    )

    # Storage overrides (optional; defaults fall back to ProjectLayout)
    workspace_dir: Path | None = Field(
        default=None,
        validation_alias=AliasChoices("workspace_dir", "WORKSPACE_DIR"),
    )
    memories_dir: Path | None = Field(
        default=None,
        validation_alias=AliasChoices("memories_dir", "MEMORIES_DIR"),
    )
    log_dir: Path | None = Field(
        default=None,
        validation_alias=AliasChoices("log_dir", "LOG_DIR"),
    )
    sessions_db_file: Path | None = Field(
        default=None,
        validation_alias=AliasChoices("sessions_db_file", "SESSIONS_DB_FILE"),
    )
```

---

## 3. Pydantic for Data Classes & Domain Modeling

Python standard library `@dataclass` lacks runtime validation, coercion, nested serialization, and strict constraint enforcement. **Prefer Pydantic `BaseModel` as the primary standard for all domain models, transfer objects, and value types.**

### Guidelines for Pydantic Models

1. **Immutability by Default (`frozen=True`)**:
   Domain data classes and value objects should be immutable. Mutating state in-place leads to race conditions and hidden side effects.
   ```python
   class UserProfile(BaseModel):
       model_config = ConfigDict(frozen=True, extra="forbid")
       user_id: str
       display_name: str
   ```
2. **Forbid Extra Fields (`extra="forbid"`)**:
   Prevent silent typos in dictionary instantiation or API payloads.
3. **Declare Explicit Field Defaults & Factories**:
   Use `Field(default=...)` for scalar primitives and `Field(default_factory=list)` (or `dict`, `set`) for mutable containers. Never use mutable objects as raw default arguments.
   ```python
   tags: list[str] = Field(default_factory=list)
   metadata: dict[str, Any] = Field(default_factory=dict)
   ```
4. **Enforce Semantic Invariants with Validators**:
   Use `@field_validator` for individual attributes and `@model_validator(mode="after")` for cross-field consistency rules.
   ```python
   from pydantic import BaseModel, Field, field_validator

   class GPSPoint(BaseModel):
       model_config = ConfigDict(frozen=True, extra="forbid")
       latitude: float = Field(ge=-90.0, le=90.0)
       longitude: float = Field(ge=-180.0, le=180.0)
       accuracy_meters: float | None = Field(default=None, ge=0.0)

       @field_validator("latitude", "longitude")
       @classmethod
       def validate_finite(cls, v: float) -> float:
           import math
           if math.isnan(v) or math.isinf(v):
               raise ValueError("Coordinate must be a finite number")
           return v
   ```

---

## 4. Making Illegal States Unrepresentable (Jane Street Principle)

> *"The central idea is this: types should be designed so that every value of the type represents a valid state in the world. If you can write down an invalid state, then someone, somewhere, will eventually construct it, and your program will misbehave."*  
> — Yaron Minsky, *Effective ML Revisited*

### The "Optional Bag & Boolean Flag" Trap (Anti-Pattern)

A common mistake is designing a single flat class with booleans and optional fields to represent multiple phases of an entity's lifecycle:

```python
# ❌ BAD: Permits contradictory, impossible states
class SessionState(BaseModel):
    is_active: bool
    is_archived: bool
    is_connecting: bool
    connection_error: str | None = None
    connected_at: datetime | None = None
    archived_at: datetime | None = None
    peer_connection_id: str | None = None
```

In this model, the type system permits $2^7$ combinations, most of which are completely illegal in reality:
- `is_active=True` AND `is_archived=True` (contradictory)
- `is_connecting=True` AND `connected_at=datetime(...)` (contradictory)
- `is_archived=False` but `archived_at=datetime(...)` (nonsense)
- `is_connecting=False` and `is_active=False` and `connection_error=None` (undefined limbo)

Every function receiving this class is forced to write defensive runtime checks:
```python
if session.is_active and not session.is_archived:
    if session.connected_at is None:
        raise RuntimeError("Impossible: active but no connected_at timestamp!")
```

### The Solution: Algebraic Discriminated Unions

Model each distinct state as its own strict Pydantic model with a discriminator tag. A state can only carry the fields relevant to that phase:

```python
from __future__ import annotations

import datetime
from typing import Annotated, Literal, Union
from pydantic import BaseModel, ConfigDict, Field


class DisconnectedState(BaseModel):
    model_config = ConfigDict(frozen=True, extra="forbid")
    status: Literal["disconnected"] = "disconnected"
    reason: str | None = None


class ConnectingState(BaseModel):
    model_config = ConfigDict(frozen=True, extra="forbid")
    status: Literal["connecting"] = "connecting"
    started_at: datetime.datetime
    attempt_count: int = Field(default=1, ge=1)


class ConnectedState(BaseModel):
    model_config = ConfigDict(frozen=True, extra="forbid")
    status: Literal["connected"] = "connected"
    connected_at: datetime.datetime
    peer_connection_id: str
    announced_ip: str


class ArchivedState(BaseModel):
    model_config = ConfigDict(frozen=True, extra="forbid")
    status: Literal["archived"] = "archived"
    archived_at: datetime.datetime
    archive_reason: str | None = None


# The discriminated union: illegal states cannot be instantiated
SessionLifecycle = Annotated[
    Union[DisconnectedState, ConnectingState, ConnectedState, ArchivedState],
    Field(discriminator="status"),
]
```

### Exhaustive Pattern Matching

Use Python 3.10+ `match / case` over the tagged discriminator. Statically verify exhaustiveness using `typing.assert_never`:

```python
from typing import assert_never


def describe_session(state: SessionLifecycle) -> str:
    match state:
        case DisconnectedState(reason=reason):
            return f"Offline ({reason})" if reason else "Offline"
        case ConnectingState(attempt_count=attempts):
            return f"Connecting (attempt {attempts})…"
        case ConnectedState(peer_connection_id=pid, announced_ip=ip):
            return f"Connected to {ip} [peer: {pid}]"
        case ArchivedState(archived_at=ts):
            return f"Archived at {ts.isoformat()}"
        case _ as unreachable:
            assert_never(unreachable)
```

### Example: Asynchronous Operation Results

```python
from typing import Generic, TypeVar

T = TypeVar("T")

class PendingResult(BaseModel):
    status: Literal["pending"] = "pending"

class RunningResult(BaseModel):
    status: Literal["running"] = "running"
    started_at: datetime.datetime
    progress_pct: float = Field(ge=0.0, le=100.0)

class SucceededResult(BaseModel, Generic[T]):
    status: Literal["succeeded"] = "succeeded"
    data: T
    completed_at: datetime.datetime

class FailedResult(BaseModel):
    status: Literal["failed"] = "failed"
    error_message: str
    failed_at: datetime.datetime
    retryable: bool = False

AsyncResult = Annotated[
    Union[PendingResult, RunningResult, SucceededResult[T], FailedResult],
    Field(discriminator="status"),
]
```
Now, a `PendingResult` can never have an `error_message`, and a `FailedResult` can never have a `data` payload. Invalid states are impossible to represent.

---

## 5. Injectable Code & Dependency Injection with `injector`

Hardcoded singletons, global instances (`env = get_env()`, `layout = MayaLayout.discover()`), and classes that instantiate their own dependencies inside `__init__` make unit testing brittle and introduce hidden coupling.

Use [Injector](https://injector.readthedocs.io/en/latest/) to practice clean **Dependency Inversion**:
1. Classes declare what they need in `__init__` using the `@inject` decorator.
2. `Module` classes define how dependencies are built and bound.
3. The `Injector` container assembles the dependency graph.

### 1. Declaring Injectable Classes

```python
from injector import inject
from maya.layout import ProjectLayout
from maya.env import Env
from maya.session.interfaces import ISessionProvider


class ChatService:
    """Business logic class: receives dependencies, never builds them."""

    @inject
    def __init__(
        self,
        layout: ProjectLayout,
        env: Env,
        session_provider: ISessionProvider,
    ) -> None:
        self._layout = layout
        self._env = env
        self._session_provider = session_provider

    def process_turn(self, session_id: str, user_text: str) -> None:
        self._session_provider.save_turn(
            session_id=session_id,
            role="user",
            text=user_text,
        )
```

### 2. Defining Dependency Modules

```python
from injector import Binder, Module, provider, singleton
from maya.env import Env
from maya.layout import ProjectLayout
from maya.session.interfaces import ISessionProvider
from maya.session.sqlite import SQLiteSessionProvider


class CoreConfigModule(Module):
    """Binds environment configuration and filesystem layout."""

    @singleton
    @provider
    def provide_env(self) -> Env:
        return Env()

    @singleton
    @provider
    def provide_layout(self, env: Env) -> ProjectLayout:
        return ProjectLayout.from_env(env)


class SessionModule(Module):
    """Binds abstract session provider to SQLite implementation."""

    def configure(self, binder: Binder) -> None:
        # Bind the interface to the concrete implementation
        binder.bind(ISessionProvider, to=SQLiteSessionProvider, scope=singleton)

    @singleton
    @provider
    def provide_sqlite_provider(self, layout: ProjectLayout) -> SQLiteSessionProvider:
        return SQLiteSessionProvider(
            sessions_dir=layout.memories_dir / "sessions",
            db_path=layout.sessions_db_file,
        )
```

### 3. Assembling the Application Container

```python
from injector import Injector

def create_container() -> Injector:
    """Build and return the dependency injection container."""
    return Injector([
        CoreConfigModule(),
        SessionModule(),
    ])
```

### 4. Avoiding Global State: Transparent Passing & Decorator Injection

> [!IMPORTANT]
> **The DI Container Exists for Transparency, Not as Ambient Global State.**
> Calling `get_container().get(...)` inside arbitrary functions or module bodies is the **Service Locator anti-pattern**—it hides dependencies and creates implicit global coupling.
> Instead, dependencies must be **transparently passed as parameters** or **injected via decorators**.

#### Pattern A: Transparent Parameter Passing with `@injectable` Defaults
Functions declare their dependencies as explicit keyword arguments. If the caller (or unit test) passes an instance, it is used directly with **zero container lookups and zero global state**. If omitted, the `@injectable` decorator transparently resolves it:

```python
from maya.di import injectable
from maya.layout import ProjectLayout

@injectable
def append_note(
    text: str,
    title: str = "",
    layout: ProjectLayout | None = None,
) -> dict:
    """Pure, testable, and transparent: layout can be passed directly or auto-injected."""
    assert layout is not None, "ProjectLayout must be provided or injected"
    layout.memories_dir.mkdir(parents=True, exist_ok=True)
    ...
```

#### Pattern B: Constructor Injection with `@inject`
Domain services and classes declare dependencies on their constructors:

```python
from injector import inject

class ChatService:
    @inject
    def __init__(self, layout: ProjectLayout, storage: ISessionProvider) -> None:
        self._layout = layout
        self._storage = storage
```

#### Pattern C: Transparent Route Injection with FastAPI `Depends`
Web endpoints declare dependencies directly in their function signatures:

```python
from fastapi import FastAPI, Depends
from injector import Injector

app = FastAPI()
container = create_container()


def get_chat_service() -> ChatService:
    """Dependency provider resolved from the container."""
    return container.get(ChatService)


@app.post("/api/chat/{session_id}")
async def send_message(
    session_id: str,
    text: str,
    service: ChatService = Depends(get_chat_service),
):
    service.process_turn(session_id, text)
    return {"ok": True}
```

### 5. Effortless Unit Testing via Container Overrides and Direct Parameter Passing

Testing becomes trivial: simply pass dependencies directly into functions or override bindings in a test container:

```python
import tempfile
from pathlib import Path
from maya.layout import ProjectLayout

def test_append_note_pure():
    with tempfile.TemporaryDirectory() as td:
        # 1. Direct, transparent parameter passing — ZERO container or global state needed!
        test_layout = ProjectLayout(root=Path(td))
        note = append_note("Buy coffee", layout=test_layout)
        assert (test_layout.notes_file).exists()

def test_chat_service_container():
    with tempfile.TemporaryDirectory() as td:
        # 2. Container override for integrated services
        test_container = Injector([
            TestConfigModule(Path(td)),
            SessionModule(),
        ])
        service = test_container.get(ChatService)
        service.process_turn("test-session", "Hello")
```

---

## 6. Anti-Patterns to Eliminate

| Anti-Pattern | Bad (Fragile / Coupled) | Good (Robust / Injectable) |
| :--- | :--- | :--- |
| **Scattered path joins** | `os.path.join(os.path.dirname(__file__), "workspace", "soul.md")` | `layout.soul_file` |
| **Magic directory strings** | `open("memories/notes.jsonl", "a")` | `open(layout.notes_file, "a")` |
| **Direct os.getenv in logic** | `os.getenv("OPENROUTER_API_KEY")` | `env.openrouter_api_key` |
| **Scattered fallback defaults** | `os.getenv("MODEL", "gemini-flash")` in 5 files | `env.gemini_model` declared once with `Field(default=...)` |
| **Mutable domain models** | Python stdlib `@dataclass` without validation or immutability | `BaseModel` with `ConfigDict(frozen=True, extra="forbid")` |
| **The Boolean/Optional Bag** | Entity with `is_connected`, `error: str \| None`, `archived_at: datetime \| None` | Tagged Discriminated Union (`Annotated[Union[A, B], Field(discriminator="status")]`) |
| **Hardcoded dependency creation** | `self.storage = SQLiteSessionProvider("./sessions.db")` in `__init__` | `@inject def __init__(self, storage: ISessionProvider):` |
| **Global mutable singleton state** | `GLOBAL_SESSIONS[sid] = {...}` loose dictionary in module root | Injectable state pool registered with `@singleton` scope |
| **Service Locator Anti-Pattern** | Calling `get_container().get(...)` inside functions and module bodies | Transparent parameters (`layout: Layout \| None = None`) or `@inject` |
| **Monkeypatching in unit tests** | `@patch.object(type(layout), "sessions_dir", ...)` | Pass parameter directly (`func(layout=test_layout)`) or test `Injector` |
