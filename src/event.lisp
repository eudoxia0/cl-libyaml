(in-package :cl-user)
(defpackage libyaml.event
  (:use :cl :cffi)
  (:import-from :libyaml.util
                :size-t)
  (:import-from :libyaml.basic
                :version-directive-t
                :tag-directive-t
                :encoding-t
                :mark-t)
  (:import-from :libyaml.style
                :scalar-style-t)
  (:export :type-t
           :stream-start-t
           :data-t
           :event-t
           :encoding
           :stream-start
           :type
           :data
           :start-mark
           :end-mark
           :event-delete))
(in-package :libyaml.event)

(defcenum type-t
  "Event types."
  :no-event
  :stream-start-event
  :stream-end-event

  :document-start-event
  :document-end-event

  :alias-event
  :scalar-event

  :mapping-start-event
  :mapping-end-event)

(defcstruct stream-start-t
  "The stream parameters (for @c YAML_STREAM_START_EVENT)."
  (encoding encoding-t))

(defcstruct tag-directives-t
  "The list of tag directives."
  (start (:pointer (:struct tag-directive-t)))
  (end (:pointer (:struct tag-directive-t))))

(defcstruct document-start-t
  "The document parameters (for @c YAML_DOCUMENT_START_EVENT)."
  (version-directive (:pointer (:struct version-directive-t)))
  (tag-directives (:struct tag-directives-t))
  (implicit :boolean))

(defcstruct document-end-t
  "The document end parameters (for @c YAML_DOCUMENT_END_EVENT)."
  (implicit :boolean))

(defcstruct alias-t
  "The alias parameters (for @c YAML_ALIAS_EVENT)."
  (anchor :string))

(defcstruct scalar-t
  "The scalar parameters (for @c YAML_SCALAR_EVENT)."
  (anchor :string)
  (tag :string)
  (value :string)
  (length size-t)
  (plain-implicit :boolean)
  (quoted-implicit :boolean)
  (style scalar-style-t))

(defcstruct sequence-start-t
  "The sequence parameters (for @c YAML_SEQUENCE_START_EVENT)."
  (anchor :string)
  (tag :string)
  (implicit :boolean)
  (style scalar-style-t))

(defcstruct mapping-start-t
  "The mapping parameters (for @c YAML_MAPPING_START_EVENT)."
  (anchor :string)
  (tag :string)
  (implicit :boolean)
  (style scalar-style-t))

(defcunion data-t
  "The event data."
  (stream-start (:struct stream-start-t))
  (document-start (:struct document-start-t))
  (document-end (:struct document-end-t))
  (alias (:struct alias-t))
  (scalar (:struct scalar-t))
  (sequence-start (:struct sequence-start-t))
  (mapping-start (:struct mapping-start-t)))

(defcstruct event-t
  "The event structure."
  (type type-t)

  (data (:union data-t))

  (start-mark (:struct mark-t))
  (end-mark (:struct mark-t)))

;; Event functions

(defcfun ("yaml_event_delete" event-delete) :void
  "Free any memory allocated for an event object."
  (token (:pointer (:struct event-t))))
