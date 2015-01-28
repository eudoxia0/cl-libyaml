(in-package :cl-user)
(defpackage libyaml.token
  (:use :cl :cffi)
  (:import-from :libyaml.basic
                :encoding-t
                :mark-t)
  (:export :type-t
           :stream-start-t
           :alias-t
           :anchor-t
           :tag-t
           :scalar-t
           :version-directive-t
           :tag-directive-t
           :data-t
           :token-t
           :token-delete))
(in-package :libyaml.token)

(defcenum type-t
  "Token types."
  :no-token

  :stream-start-token
  :stream-end-token

  :version-directory-token
  :tag-directive-token
  :document-start-token
  :document-end-token

  :block-sequence-start-token
  :block-mapping-start-token
  :block-end-token

  :flow-sequence-start-token
  :flow-sequence-end-token
  :flow-mapping-start-token
  :flow-mapping-end-token

  :block-entry-token
  :flow-entry-token
  :key-token
  :value-token

  :alias-token
  :anchor-token
  :tag-token
  :scalar-token)

(defcstruct stream-start-t
  "The stream start (for @c YAML_STREAM_START_TOKEN)."
  (encoding encoding-t))

(defcstruct alias-t
  "The alias (for @c YAML_ALIAS_TOKEN)."
  (value :string))

(defcstruct anchor-t
  "The anchor (for @c YAML_ANCHOR_TOKEN)."
  (value :string))

(defcstruct tag-t
  "The tag (for @c YAML_TAG_TOKEN)."
  (handle :string)
  (suffix :string))

(defcstruct scalar-t
  "The scalar value (for @c YAML_SCALAR_TOKEN)."
  (value :string))

(defcstruct version-directive-t
  "The version directive (for @c YAML_VERSION_DIRECTIVE_TOKEN)."
  (major :int)
  (minor :int))

(defcstruct tag-directive-t
  "The tag directive (for @c YAML_TAG_DIRECTIVE_TOKEN)."
  (handle :string)
  (prefix :string))

(defcunion data-t
  "The token data."
  (stream-start (:struct stream-start-t))
  (alias (:struct alias-t))
  (anchor (:struct anchor-t))
  (tag (:struct tag-t))
  (scalar (:struct scalar-t))
  (version-directive (:struct version-directive-t))
  (tag-directive (:struct tag-directive-t)))

(defcstruct token-t
  "The token structure."
  (type type-t)

  (data (:union data-t))

  (start-mark (:struct mark-t))
  (end-mark (:struct mark-t)))

;; Token functions

(defcfun ("yaml_token_delete" token-delete) :void
  "Free any memory allocated for a token object."
  (token (:pointer (:struct token-t))))
