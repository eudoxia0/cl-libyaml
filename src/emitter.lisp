(in-package :cl-user)
(defpackage libyaml.emitter
  (:use :cl :cffi)
  (:import-from :libyaml.util
		:size-t)
  (:import-from :libyaml.basic
		:break-t
		:error-type-t
		:encoding-t
		:tag-directive-t)
  (:import-from :libyaml.event
		:event-t)
  (:import-from :libyaml.document
		:document-t)
  (:import-from :libyaml.style
		:scalar-style-t
		:sequence-style-t
		:mapping-style-t)
  (:import-from :libyaml.version
		:version-directive-t)
  (:export :write-handler-t
           :state-t
	   :buffer-t
	   :raw-buffer-t
	   :indent-stack-t
	   :tag-directives-t
	   :emitter-states-stack-t
	   :event-queue-t
	   :anchor-data-t
	   :tag-data-t
	   :scalar-data-t
	   :anchor-info-t
	   :output-string-t
	   :output-t
	   :emitter-t
	   :allocate-emitter
	   :emitter-initialize
	   :emitter-delete
	   :set-output-string
	   :set-output-file
	   :set-output
	   :emit
	   :emitter-open
	   :emitter-close
	   :emitter-dump
	   :emitter-flush
	   :stream-start-event-initialize
	   :stream-end-event-initialize
	   :document-start-event-initialize
	   :document-end-event-initialize
	   :scalar-event-initialize
	   :sequence-start-event-initialize
	   :sequence-end-event-initialize
	   :mapping-start-event-initialize
	   :mapping-end-event-initialize)
  (:documentation "The libyaml emitter. This package is incomplete."))
(in-package :libyaml.emitter)

(defctype write-handler-t :pointer)

(defcenum state-t
  "The emitter states."
  :emit-stream-start-state
  :emit-first-document-start-state
  :emit-document-start-state
  :emit-document-content-state
  :emit-document-end-state
  :emit-flow-sequence-first-item-state
  :emit-flow-sequence-item-state
  :emit-flow-mapping-first-key-state
  :emit-flow-mapping-key-state
  :emit-flow-mapping-simple-value-state
  :emit-flow-mapping-value-state
  :emit-block-sequence-first-item-state
  :emit-block-sequence-item-state
  :emit-block-mapping-first-key-state
  :emit-block-mapping-key-state
  :emit-block-mapping-simple-value-state
  :emit-block-mapping-value-state
  :emit-end-state)

(defcstruct buffer-t
  "Working character buffer type"
  (start :pointer) ; yaml_char_t
  (end :pointer)
  (pointer :pointer)
  (last :pointer))

(defcstruct raw-buffer-t
  "Raw character buffer type"
  (start :pointer) ; unsigned char
  (end :pointer)
  (pointer :pointer)
  (last :pointer))

(defcstruct indent-stack-t
  "The indentation levels stack."
  (start (:pointer :int))
  (end (:pointer :int))
  (top (:pointer :int)))

(defcstruct tag-directives-t
  "The list of TAG directives."
  (start (:pointer (:struct tag-directive-t)))
  (end (:pointer (:struct tag-directive-t)))
  (top (:pointer (:struct tag-directive-t))))

(defcstruct emitter-states-stack-t
  "The emitter states stack"
  (start (:pointer state-t))
  (end (:pointer state-t))
  (top (:pointer state-t)))

(defcstruct event-queue-t
  "The event queue"
  (start (:pointer (:struct event-t)))
  (end (:pointer (:struct event-t)))
  (head (:pointer (:struct event-t)))
  (tail (:pointer (:struct event-t))))

(defcstruct anchor-data-t
  "Anchor analysis"
  (anchor :pointer)
  (anchor-length size-t)
  (alias :int))

(defcstruct tag-data-t
  "Tag analysis"
  (handle :pointer)
  (handle-length size-t)
  (suffix :pointer)
  (suffix-length size-t))

(defcstruct scalar-data-t
  "Scalar analysis"
  (value :pointer)
  (length size-t)
  (multiline :int)
  (flow-plain-allowed :int)
  (block-plain-allowed :int)
  (single-quoted-allowed :int)
  (block-allowed :int)
  (style scalar-style-t))

(defcstruct anchor-info-t
  "The information associated with the document notes"
  (references :int)
  (anchor :int)
  (serialized-p :int))

(defcstruct output-string-t
  "String output data"
  (buffer :pointer)
  (size size-t)
  (size-written (:pointer size-t)))

(defcunion output-t
  "Standard (string for file) output data"
  (string (:struct output-string-t))
  (file :pointer))

(defcstruct emitter-t
  "The emitter structure"
  ;; Error handling
  (error error-type-t)
  (problem :string)
  ;; Writer
  (write-handler (:pointer write-handler-t))
  (write-handler-data :pointer)
  (output (:union output-t))
  (buffer (:struct buffer-t))
  (raw-buffer (:struct raw-buffer-t))
  (encoding encoding-t)
  ;; Emitter
  (canonical :int)
  (best-indent :int)
  (best-width :int)
  (unicode :int)
  (line-break break-t)
  (states (:struct emitter-states-stack-t))
  (state state-t)
  (events (:struct event-queue-t))
  (indents (:struct indent-stack-t))
  (tag-directives (:struct tag-directives-t))
  (indent :int)
  (flow-level :int)
  (root-context :int)
  (sequence-context :int)
  (mapping-context :int)
  (simple-key-context :int)
  (line :int)
  (column :int)
  (whitespace :int)
  (indentation :int)
  (open-ended :int)
  (anchor-data (:struct anchor-data-t))
  (tag-data (:struct tag-data-t))
  (scalar-data (:struct scalar-data-t))
  ;; Dumper
  (opened-p :int)
  (closed-p :int)
  (anchors (:pointer (:struct anchor-info-t)))
  (last-anchor-id :int)
  (document (:pointer (:struct document-t))))

(defun allocate-emitter ()
  (foreign-alloc '(:struct emitter-t)))

(defcfun ("yaml_emitter_initialize" emitter-initialize) :int
  "Initialize an emitter."
  (emitter (:pointer (:struct emitter-t))))

(defcfun ("yaml_emitter_delete" emitter-delete) :void
  "Destroy an emitter."
  (emitter (:pointer (:struct emitter-t))))

(defcfun ("yaml_emitter_set_output_string" set-output-string) :void
  "Set a string output."
  (emitter (:pointer (:struct emitter-t)))
  (output (:pointer))
  (size size-t)
  (size-written (:pointer size-t)))

(defcfun ("yaml_emitter_set_output_file" set-output-file) :void
  "Set a file output."
  (emitter (:pointer (:struct emitter-t)))
  (file :pointer))

(defcfun ("yaml_emitter_set_output" set-output) :void
  "Set a generic output handler."
  (emitter (:pointer (:struct emitter-t)))
  (handler (:pointer (write-handler-t)))
  (data :pointer))

(defcfun ("yaml_emitter_emit" emit) :int
  "Emit an event."
  (emitter (:pointer (:struct emitter-t)))
  (event (:pointer (:struct event-t))))

(defcfun ("yaml_emitter_open" emitter-open) :int
  "Start a YAML stream."
  (emitter (:pointer (:struct emitter-t))))

(defcfun ("yaml_emitter_close" emitter-close) :int
  "Finish a YAML stream."
  (emitter (:pointer (:struct emitter-t))))

(defcfun ("yaml_emitter_dump" emitter-dump) :int
  "Exit a YAML document."
  (emitter (:pointer (:struct emitter-t)))
  (document (:pointer (:struct document-t))))

(defcfun ("yaml_emitter_flush" emitter-flush) :int
  "Flush the accumulated characters to the output."
  (emitter (:pointer (:struct emitter-t))))

;; These are event functions, but most likely used while emitting

(defcfun ("yaml_stream_start_event_initialize" stream-start-event-initialize) :int
  "Create a STREAM-START event."
  (event (:pointer (:struct event-t)))
  (encoding encoding-t))

(defcfun ("yaml_stream_end_event_initialize" stream-end-event-initialize) :int
  "Create a STREAM-END event."
  (event (:pointer (:struct event-t))))

(defcfun ("yaml_document_start_event_initialize" document-start-event-initialize) :int
  "Create the DOCUMENT-START event."
  (event (:pointer (:struct event-t)))
  (version-directive (:pointer (:struct version-directive-t)))
  (tag-directive-start (:pointer (:struct tag-directive-t)))
  (tag-directive-end (:pointer (:struct tag-directive-t)))
  (implicit :boolean))

(defcfun ("yaml_document_end_event_initialize" document-end-event-initialize) :int
  "Create the DOCUMENT-END event."
  (event (:pointer (:struct event-t)))
  (implicit :boolean))

(defcfun ("yaml_alias_event_initialize" alias-event-initialize) :int
  "Create an ALIAS event."
  (event (:pointer (:struct event-t)))
  (anchor :string))

(defcfun ("yaml_scalar_event_initialize" scalar-event-initialize) :int
  (event (:pointer (:struct event-t)))
  (anchor :string)
  (tag :string)
  (value :string)
  (length :int)
  (plain-implicit :boolean)
  (quoted-implicit :boolean)
  (style scalar-style-t))

(defcfun ("yaml_sequence_start_event_initialize" sequence-start-event-initialize) :int
  (event (:pointer (:struct event-t)))
  (anchor :string)
  (tag :string)
  (implicit :boolean)
  (style sequence-style-t))

(defcfun ("yaml_sequence_end_event_initialize" sequence-end-event-initialize) :int
  (event (:pointer (:struct event-t))))

(defcfun ("yaml_mapping_start_event_initialize" mapping-start-event-initialize) :int
  (event (:pointer (:struct event-t)))
  (anchor :string)
  (tag :string)
  (implicit :boolean)
  (style mapping-style-t))

(defcfun ("yaml_mapping_end_event_initialize" mapping-end-event-initialize) :int
  (event (:pointer (:struct event-t))))
