"""Task storage backends, layouts, and YAML document I/O."""

from pi_job_harness.store.factory import (
    layout_for_document_path,
    open_task_store,
    project,
    unsupported_storage,
)
from pi_job_harness.store.fs import FsTaskStore, TaskLayout
from pi_job_harness.store.protocol import TaskStore
from pi_job_harness.store.yaml import BundleTaskLayout, YamlTaskLayout, YamlTaskStore
from pi_job_harness.store.yaml_io import (
    UniqueKeyLoader,
    atomic_create_text,
    atomic_write_text,
    canonical_task_mapping,
    compute_content_digest,
    is_content_dirty,
    load_yaml_mapping,
    render_yaml_task,
    semantic_task_mapping,
    set_content_digest,
    validate_task_mapping,
    warn_if_content_dirty,
    yaml_task_lock_path,
)

__all__ = [
    "BundleTaskLayout",
    "FsTaskStore",
    "TaskLayout",
    "TaskStore",
    "UniqueKeyLoader",
    "YamlTaskLayout",
    "YamlTaskStore",
    "atomic_create_text",
    "atomic_write_text",
    "canonical_task_mapping",
    "compute_content_digest",
    "is_content_dirty",
    "layout_for_document_path",
    "load_yaml_mapping",
    "open_task_store",
    "project",
    "render_yaml_task",
    "semantic_task_mapping",
    "set_content_digest",
    "unsupported_storage",
    "validate_task_mapping",
    "warn_if_content_dirty",
    "yaml_task_lock_path",
]
