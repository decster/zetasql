// Generated by the protocol buffer compiler.  DO NOT EDIT!
// source: zetasql/public/annotation.proto

#ifndef GOOGLE_PROTOBUF_INCLUDED_zetasql_2fpublic_2fannotation_2eproto
#define GOOGLE_PROTOBUF_INCLUDED_zetasql_2fpublic_2fannotation_2eproto

#include <limits>
#include <string>

#include <google/protobuf/port_def.inc>
#if PROTOBUF_VERSION < 3019000
#error This file was generated by a newer version of protoc which is
#error incompatible with your Protocol Buffer headers. Please update
#error your headers.
#endif
#if 3019001 < PROTOBUF_MIN_PROTOC_VERSION
#error This file was generated by an older version of protoc which is
#error incompatible with your Protocol Buffer headers. Please
#error regenerate this file with a newer version of protoc.
#endif

#include <google/protobuf/port_undef.inc>
#include <google/protobuf/io/coded_stream.h>
#include <google/protobuf/arena.h>
#include <google/protobuf/arenastring.h>
#include <google/protobuf/generated_message_table_driven.h>
#include <google/protobuf/generated_message_util.h>
#include <google/protobuf/metadata_lite.h>
#include <google/protobuf/generated_message_reflection.h>
#include <google/protobuf/message.h>
#include <google/protobuf/repeated_field.h>  // IWYU pragma: export
#include <google/protobuf/extension_set.h>  // IWYU pragma: export
#include <google/protobuf/unknown_field_set.h>
#include "zetasql/public/simple_value.pb.h"
// @@protoc_insertion_point(includes)
#include <google/protobuf/port_def.inc>
#define PROTOBUF_INTERNAL_EXPORT_zetasql_2fpublic_2fannotation_2eproto
PROTOBUF_NAMESPACE_OPEN
namespace internal {
class AnyMetadata;
}  // namespace internal
PROTOBUF_NAMESPACE_CLOSE

// Internal implementation detail -- do not use these members.
struct TableStruct_zetasql_2fpublic_2fannotation_2eproto {
  static const ::PROTOBUF_NAMESPACE_ID::internal::ParseTableField entries[]
    PROTOBUF_SECTION_VARIABLE(protodesc_cold);
  static const ::PROTOBUF_NAMESPACE_ID::internal::AuxiliaryParseTableField aux[]
    PROTOBUF_SECTION_VARIABLE(protodesc_cold);
  static const ::PROTOBUF_NAMESPACE_ID::internal::ParseTable schema[2]
    PROTOBUF_SECTION_VARIABLE(protodesc_cold);
  static const ::PROTOBUF_NAMESPACE_ID::internal::FieldMetadata field_metadata[];
  static const ::PROTOBUF_NAMESPACE_ID::internal::SerializationTable serialization_table[];
  static const uint32_t offsets[];
};
extern const ::PROTOBUF_NAMESPACE_ID::internal::DescriptorTable descriptor_table_zetasql_2fpublic_2fannotation_2eproto;
namespace zetasql {
class AnnotationMapProto;
struct AnnotationMapProtoDefaultTypeInternal;
extern AnnotationMapProtoDefaultTypeInternal _AnnotationMapProto_default_instance_;
class AnnotationProto;
struct AnnotationProtoDefaultTypeInternal;
extern AnnotationProtoDefaultTypeInternal _AnnotationProto_default_instance_;
}  // namespace zetasql
PROTOBUF_NAMESPACE_OPEN
template<> ::zetasql::AnnotationMapProto* Arena::CreateMaybeMessage<::zetasql::AnnotationMapProto>(Arena*);
template<> ::zetasql::AnnotationProto* Arena::CreateMaybeMessage<::zetasql::AnnotationProto>(Arena*);
PROTOBUF_NAMESPACE_CLOSE
namespace zetasql {

// ===================================================================

class AnnotationProto final :
    public ::PROTOBUF_NAMESPACE_ID::Message /* @@protoc_insertion_point(class_definition:zetasql.AnnotationProto) */ {
 public:
  inline AnnotationProto() : AnnotationProto(nullptr) {}
  ~AnnotationProto() override;
  explicit constexpr AnnotationProto(::PROTOBUF_NAMESPACE_ID::internal::ConstantInitialized);

  AnnotationProto(const AnnotationProto& from);
  AnnotationProto(AnnotationProto&& from) noexcept
    : AnnotationProto() {
    *this = ::std::move(from);
  }

  inline AnnotationProto& operator=(const AnnotationProto& from) {
    CopyFrom(from);
    return *this;
  }
  inline AnnotationProto& operator=(AnnotationProto&& from) noexcept {
    if (this == &from) return *this;
    if (GetOwningArena() == from.GetOwningArena()
  #ifdef PROTOBUF_FORCE_COPY_IN_MOVE
        && GetOwningArena() != nullptr
  #endif  // !PROTOBUF_FORCE_COPY_IN_MOVE
    ) {
      InternalSwap(&from);
    } else {
      CopyFrom(from);
    }
    return *this;
  }

  inline const ::PROTOBUF_NAMESPACE_ID::UnknownFieldSet& unknown_fields() const {
    return _internal_metadata_.unknown_fields<::PROTOBUF_NAMESPACE_ID::UnknownFieldSet>(::PROTOBUF_NAMESPACE_ID::UnknownFieldSet::default_instance);
  }
  inline ::PROTOBUF_NAMESPACE_ID::UnknownFieldSet* mutable_unknown_fields() {
    return _internal_metadata_.mutable_unknown_fields<::PROTOBUF_NAMESPACE_ID::UnknownFieldSet>();
  }

  static const ::PROTOBUF_NAMESPACE_ID::Descriptor* descriptor() {
    return GetDescriptor();
  }
  static const ::PROTOBUF_NAMESPACE_ID::Descriptor* GetDescriptor() {
    return default_instance().GetMetadata().descriptor;
  }
  static const ::PROTOBUF_NAMESPACE_ID::Reflection* GetReflection() {
    return default_instance().GetMetadata().reflection;
  }
  static const AnnotationProto& default_instance() {
    return *internal_default_instance();
  }
  static inline const AnnotationProto* internal_default_instance() {
    return reinterpret_cast<const AnnotationProto*>(
               &_AnnotationProto_default_instance_);
  }
  static constexpr int kIndexInFileMessages =
    0;

  friend void swap(AnnotationProto& a, AnnotationProto& b) {
    a.Swap(&b);
  }
  inline void Swap(AnnotationProto* other) {
    if (other == this) return;
  #ifdef PROTOBUF_FORCE_COPY_IN_SWAP
    if (GetOwningArena() != nullptr &&
        GetOwningArena() == other->GetOwningArena()) {
   #else  // PROTOBUF_FORCE_COPY_IN_SWAP
    if (GetOwningArena() == other->GetOwningArena()) {
  #endif  // !PROTOBUF_FORCE_COPY_IN_SWAP
      InternalSwap(other);
    } else {
      ::PROTOBUF_NAMESPACE_ID::internal::GenericSwap(this, other);
    }
  }
  void UnsafeArenaSwap(AnnotationProto* other) {
    if (other == this) return;
    GOOGLE_DCHECK(GetOwningArena() == other->GetOwningArena());
    InternalSwap(other);
  }

  // implements Message ----------------------------------------------

  AnnotationProto* New(::PROTOBUF_NAMESPACE_ID::Arena* arena = nullptr) const final {
    return CreateMaybeMessage<AnnotationProto>(arena);
  }
  using ::PROTOBUF_NAMESPACE_ID::Message::CopyFrom;
  void CopyFrom(const AnnotationProto& from);
  using ::PROTOBUF_NAMESPACE_ID::Message::MergeFrom;
  void MergeFrom(const AnnotationProto& from);
  private:
  static void MergeImpl(::PROTOBUF_NAMESPACE_ID::Message* to, const ::PROTOBUF_NAMESPACE_ID::Message& from);
  public:
  PROTOBUF_ATTRIBUTE_REINITIALIZES void Clear() final;
  bool IsInitialized() const final;

  size_t ByteSizeLong() const final;
  const char* _InternalParse(const char* ptr, ::PROTOBUF_NAMESPACE_ID::internal::ParseContext* ctx) final;
  uint8_t* _InternalSerialize(
      uint8_t* target, ::PROTOBUF_NAMESPACE_ID::io::EpsCopyOutputStream* stream) const final;
  int GetCachedSize() const final { return _cached_size_.Get(); }

  private:
  void SharedCtor();
  void SharedDtor();
  void SetCachedSize(int size) const final;
  void InternalSwap(AnnotationProto* other);

  private:
  friend class ::PROTOBUF_NAMESPACE_ID::internal::AnyMetadata;
  static ::PROTOBUF_NAMESPACE_ID::StringPiece FullMessageName() {
    return "zetasql.AnnotationProto";
  }
  protected:
  explicit AnnotationProto(::PROTOBUF_NAMESPACE_ID::Arena* arena,
                       bool is_message_owned = false);
  private:
  static void ArenaDtor(void* object);
  inline void RegisterArenaDtor(::PROTOBUF_NAMESPACE_ID::Arena* arena);
  public:

  static const ClassData _class_data_;
  const ::PROTOBUF_NAMESPACE_ID::Message::ClassData*GetClassData() const final;

  ::PROTOBUF_NAMESPACE_ID::Metadata GetMetadata() const final;

  // nested types ----------------------------------------------------

  // accessors -------------------------------------------------------

  enum : int {
    kValueFieldNumber = 2,
    kIdFieldNumber = 1,
  };
  // optional .zetasql.SimpleValueProto value = 2;
  bool has_value() const;
  private:
  bool _internal_has_value() const;
  public:
  void clear_value();
  const ::zetasql::SimpleValueProto& value() const;
  PROTOBUF_NODISCARD ::zetasql::SimpleValueProto* release_value();
  ::zetasql::SimpleValueProto* mutable_value();
  void set_allocated_value(::zetasql::SimpleValueProto* value);
  private:
  const ::zetasql::SimpleValueProto& _internal_value() const;
  ::zetasql::SimpleValueProto* _internal_mutable_value();
  public:
  void unsafe_arena_set_allocated_value(
      ::zetasql::SimpleValueProto* value);
  ::zetasql::SimpleValueProto* unsafe_arena_release_value();

  // optional int64 id = 1;
  bool has_id() const;
  private:
  bool _internal_has_id() const;
  public:
  void clear_id();
  int64_t id() const;
  void set_id(int64_t value);
  private:
  int64_t _internal_id() const;
  void _internal_set_id(int64_t value);
  public:

  // @@protoc_insertion_point(class_scope:zetasql.AnnotationProto)
 private:
  class _Internal;

  template <typename T> friend class ::PROTOBUF_NAMESPACE_ID::Arena::InternalHelper;
  typedef void InternalArenaConstructable_;
  typedef void DestructorSkippable_;
  ::PROTOBUF_NAMESPACE_ID::internal::HasBits<1> _has_bits_;
  mutable ::PROTOBUF_NAMESPACE_ID::internal::CachedSize _cached_size_;
  ::zetasql::SimpleValueProto* value_;
  int64_t id_;
  friend struct ::TableStruct_zetasql_2fpublic_2fannotation_2eproto;
};
// -------------------------------------------------------------------

class AnnotationMapProto final :
    public ::PROTOBUF_NAMESPACE_ID::Message /* @@protoc_insertion_point(class_definition:zetasql.AnnotationMapProto) */ {
 public:
  inline AnnotationMapProto() : AnnotationMapProto(nullptr) {}
  ~AnnotationMapProto() override;
  explicit constexpr AnnotationMapProto(::PROTOBUF_NAMESPACE_ID::internal::ConstantInitialized);

  AnnotationMapProto(const AnnotationMapProto& from);
  AnnotationMapProto(AnnotationMapProto&& from) noexcept
    : AnnotationMapProto() {
    *this = ::std::move(from);
  }

  inline AnnotationMapProto& operator=(const AnnotationMapProto& from) {
    CopyFrom(from);
    return *this;
  }
  inline AnnotationMapProto& operator=(AnnotationMapProto&& from) noexcept {
    if (this == &from) return *this;
    if (GetOwningArena() == from.GetOwningArena()
  #ifdef PROTOBUF_FORCE_COPY_IN_MOVE
        && GetOwningArena() != nullptr
  #endif  // !PROTOBUF_FORCE_COPY_IN_MOVE
    ) {
      InternalSwap(&from);
    } else {
      CopyFrom(from);
    }
    return *this;
  }

  inline const ::PROTOBUF_NAMESPACE_ID::UnknownFieldSet& unknown_fields() const {
    return _internal_metadata_.unknown_fields<::PROTOBUF_NAMESPACE_ID::UnknownFieldSet>(::PROTOBUF_NAMESPACE_ID::UnknownFieldSet::default_instance);
  }
  inline ::PROTOBUF_NAMESPACE_ID::UnknownFieldSet* mutable_unknown_fields() {
    return _internal_metadata_.mutable_unknown_fields<::PROTOBUF_NAMESPACE_ID::UnknownFieldSet>();
  }

  static const ::PROTOBUF_NAMESPACE_ID::Descriptor* descriptor() {
    return GetDescriptor();
  }
  static const ::PROTOBUF_NAMESPACE_ID::Descriptor* GetDescriptor() {
    return default_instance().GetMetadata().descriptor;
  }
  static const ::PROTOBUF_NAMESPACE_ID::Reflection* GetReflection() {
    return default_instance().GetMetadata().reflection;
  }
  static const AnnotationMapProto& default_instance() {
    return *internal_default_instance();
  }
  static inline const AnnotationMapProto* internal_default_instance() {
    return reinterpret_cast<const AnnotationMapProto*>(
               &_AnnotationMapProto_default_instance_);
  }
  static constexpr int kIndexInFileMessages =
    1;

  friend void swap(AnnotationMapProto& a, AnnotationMapProto& b) {
    a.Swap(&b);
  }
  inline void Swap(AnnotationMapProto* other) {
    if (other == this) return;
  #ifdef PROTOBUF_FORCE_COPY_IN_SWAP
    if (GetOwningArena() != nullptr &&
        GetOwningArena() == other->GetOwningArena()) {
   #else  // PROTOBUF_FORCE_COPY_IN_SWAP
    if (GetOwningArena() == other->GetOwningArena()) {
  #endif  // !PROTOBUF_FORCE_COPY_IN_SWAP
      InternalSwap(other);
    } else {
      ::PROTOBUF_NAMESPACE_ID::internal::GenericSwap(this, other);
    }
  }
  void UnsafeArenaSwap(AnnotationMapProto* other) {
    if (other == this) return;
    GOOGLE_DCHECK(GetOwningArena() == other->GetOwningArena());
    InternalSwap(other);
  }

  // implements Message ----------------------------------------------

  AnnotationMapProto* New(::PROTOBUF_NAMESPACE_ID::Arena* arena = nullptr) const final {
    return CreateMaybeMessage<AnnotationMapProto>(arena);
  }
  using ::PROTOBUF_NAMESPACE_ID::Message::CopyFrom;
  void CopyFrom(const AnnotationMapProto& from);
  using ::PROTOBUF_NAMESPACE_ID::Message::MergeFrom;
  void MergeFrom(const AnnotationMapProto& from);
  private:
  static void MergeImpl(::PROTOBUF_NAMESPACE_ID::Message* to, const ::PROTOBUF_NAMESPACE_ID::Message& from);
  public:
  PROTOBUF_ATTRIBUTE_REINITIALIZES void Clear() final;
  bool IsInitialized() const final;

  size_t ByteSizeLong() const final;
  const char* _InternalParse(const char* ptr, ::PROTOBUF_NAMESPACE_ID::internal::ParseContext* ctx) final;
  uint8_t* _InternalSerialize(
      uint8_t* target, ::PROTOBUF_NAMESPACE_ID::io::EpsCopyOutputStream* stream) const final;
  int GetCachedSize() const final { return _cached_size_.Get(); }

  private:
  void SharedCtor();
  void SharedDtor();
  void SetCachedSize(int size) const final;
  void InternalSwap(AnnotationMapProto* other);

  private:
  friend class ::PROTOBUF_NAMESPACE_ID::internal::AnyMetadata;
  static ::PROTOBUF_NAMESPACE_ID::StringPiece FullMessageName() {
    return "zetasql.AnnotationMapProto";
  }
  protected:
  explicit AnnotationMapProto(::PROTOBUF_NAMESPACE_ID::Arena* arena,
                       bool is_message_owned = false);
  private:
  static void ArenaDtor(void* object);
  inline void RegisterArenaDtor(::PROTOBUF_NAMESPACE_ID::Arena* arena);
  public:

  static const ClassData _class_data_;
  const ::PROTOBUF_NAMESPACE_ID::Message::ClassData*GetClassData() const final;

  ::PROTOBUF_NAMESPACE_ID::Metadata GetMetadata() const final;

  // nested types ----------------------------------------------------

  // accessors -------------------------------------------------------

  enum : int {
    kAnnotationsFieldNumber = 1,
    kStructFieldsFieldNumber = 3,
    kArrayElementFieldNumber = 2,
    kIsNullFieldNumber = 4,
  };
  // repeated .zetasql.AnnotationProto annotations = 1;
  int annotations_size() const;
  private:
  int _internal_annotations_size() const;
  public:
  void clear_annotations();
  ::zetasql::AnnotationProto* mutable_annotations(int index);
  ::PROTOBUF_NAMESPACE_ID::RepeatedPtrField< ::zetasql::AnnotationProto >*
      mutable_annotations();
  private:
  const ::zetasql::AnnotationProto& _internal_annotations(int index) const;
  ::zetasql::AnnotationProto* _internal_add_annotations();
  public:
  const ::zetasql::AnnotationProto& annotations(int index) const;
  ::zetasql::AnnotationProto* add_annotations();
  const ::PROTOBUF_NAMESPACE_ID::RepeatedPtrField< ::zetasql::AnnotationProto >&
      annotations() const;

  // repeated .zetasql.AnnotationMapProto struct_fields = 3;
  int struct_fields_size() const;
  private:
  int _internal_struct_fields_size() const;
  public:
  void clear_struct_fields();
  ::zetasql::AnnotationMapProto* mutable_struct_fields(int index);
  ::PROTOBUF_NAMESPACE_ID::RepeatedPtrField< ::zetasql::AnnotationMapProto >*
      mutable_struct_fields();
  private:
  const ::zetasql::AnnotationMapProto& _internal_struct_fields(int index) const;
  ::zetasql::AnnotationMapProto* _internal_add_struct_fields();
  public:
  const ::zetasql::AnnotationMapProto& struct_fields(int index) const;
  ::zetasql::AnnotationMapProto* add_struct_fields();
  const ::PROTOBUF_NAMESPACE_ID::RepeatedPtrField< ::zetasql::AnnotationMapProto >&
      struct_fields() const;

  // optional .zetasql.AnnotationMapProto array_element = 2;
  bool has_array_element() const;
  private:
  bool _internal_has_array_element() const;
  public:
  void clear_array_element();
  const ::zetasql::AnnotationMapProto& array_element() const;
  PROTOBUF_NODISCARD ::zetasql::AnnotationMapProto* release_array_element();
  ::zetasql::AnnotationMapProto* mutable_array_element();
  void set_allocated_array_element(::zetasql::AnnotationMapProto* array_element);
  private:
  const ::zetasql::AnnotationMapProto& _internal_array_element() const;
  ::zetasql::AnnotationMapProto* _internal_mutable_array_element();
  public:
  void unsafe_arena_set_allocated_array_element(
      ::zetasql::AnnotationMapProto* array_element);
  ::zetasql::AnnotationMapProto* unsafe_arena_release_array_element();

  // optional bool is_null = 4;
  bool has_is_null() const;
  private:
  bool _internal_has_is_null() const;
  public:
  void clear_is_null();
  bool is_null() const;
  void set_is_null(bool value);
  private:
  bool _internal_is_null() const;
  void _internal_set_is_null(bool value);
  public:

  // @@protoc_insertion_point(class_scope:zetasql.AnnotationMapProto)
 private:
  class _Internal;

  template <typename T> friend class ::PROTOBUF_NAMESPACE_ID::Arena::InternalHelper;
  typedef void InternalArenaConstructable_;
  typedef void DestructorSkippable_;
  ::PROTOBUF_NAMESPACE_ID::internal::HasBits<1> _has_bits_;
  mutable ::PROTOBUF_NAMESPACE_ID::internal::CachedSize _cached_size_;
  ::PROTOBUF_NAMESPACE_ID::RepeatedPtrField< ::zetasql::AnnotationProto > annotations_;
  ::PROTOBUF_NAMESPACE_ID::RepeatedPtrField< ::zetasql::AnnotationMapProto > struct_fields_;
  ::zetasql::AnnotationMapProto* array_element_;
  bool is_null_;
  friend struct ::TableStruct_zetasql_2fpublic_2fannotation_2eproto;
};
// ===================================================================


// ===================================================================

#ifdef __GNUC__
  #pragma GCC diagnostic push
  #pragma GCC diagnostic ignored "-Wstrict-aliasing"
#endif  // __GNUC__
// AnnotationProto

// optional int64 id = 1;
inline bool AnnotationProto::_internal_has_id() const {
  bool value = (_has_bits_[0] & 0x00000002u) != 0;
  return value;
}
inline bool AnnotationProto::has_id() const {
  return _internal_has_id();
}
inline void AnnotationProto::clear_id() {
  id_ = int64_t{0};
  _has_bits_[0] &= ~0x00000002u;
}
inline int64_t AnnotationProto::_internal_id() const {
  return id_;
}
inline int64_t AnnotationProto::id() const {
  // @@protoc_insertion_point(field_get:zetasql.AnnotationProto.id)
  return _internal_id();
}
inline void AnnotationProto::_internal_set_id(int64_t value) {
  _has_bits_[0] |= 0x00000002u;
  id_ = value;
}
inline void AnnotationProto::set_id(int64_t value) {
  _internal_set_id(value);
  // @@protoc_insertion_point(field_set:zetasql.AnnotationProto.id)
}

// optional .zetasql.SimpleValueProto value = 2;
inline bool AnnotationProto::_internal_has_value() const {
  bool value = (_has_bits_[0] & 0x00000001u) != 0;
  PROTOBUF_ASSUME(!value || value_ != nullptr);
  return value;
}
inline bool AnnotationProto::has_value() const {
  return _internal_has_value();
}
inline const ::zetasql::SimpleValueProto& AnnotationProto::_internal_value() const {
  const ::zetasql::SimpleValueProto* p = value_;
  return p != nullptr ? *p : reinterpret_cast<const ::zetasql::SimpleValueProto&>(
      ::zetasql::_SimpleValueProto_default_instance_);
}
inline const ::zetasql::SimpleValueProto& AnnotationProto::value() const {
  // @@protoc_insertion_point(field_get:zetasql.AnnotationProto.value)
  return _internal_value();
}
inline void AnnotationProto::unsafe_arena_set_allocated_value(
    ::zetasql::SimpleValueProto* value) {
  if (GetArenaForAllocation() == nullptr) {
    delete reinterpret_cast<::PROTOBUF_NAMESPACE_ID::MessageLite*>(value_);
  }
  value_ = value;
  if (value) {
    _has_bits_[0] |= 0x00000001u;
  } else {
    _has_bits_[0] &= ~0x00000001u;
  }
  // @@protoc_insertion_point(field_unsafe_arena_set_allocated:zetasql.AnnotationProto.value)
}
inline ::zetasql::SimpleValueProto* AnnotationProto::release_value() {
  _has_bits_[0] &= ~0x00000001u;
  ::zetasql::SimpleValueProto* temp = value_;
  value_ = nullptr;
#ifdef PROTOBUF_FORCE_COPY_IN_RELEASE
  auto* old =  reinterpret_cast<::PROTOBUF_NAMESPACE_ID::MessageLite*>(temp);
  temp = ::PROTOBUF_NAMESPACE_ID::internal::DuplicateIfNonNull(temp);
  if (GetArenaForAllocation() == nullptr) { delete old; }
#else  // PROTOBUF_FORCE_COPY_IN_RELEASE
  if (GetArenaForAllocation() != nullptr) {
    temp = ::PROTOBUF_NAMESPACE_ID::internal::DuplicateIfNonNull(temp);
  }
#endif  // !PROTOBUF_FORCE_COPY_IN_RELEASE
  return temp;
}
inline ::zetasql::SimpleValueProto* AnnotationProto::unsafe_arena_release_value() {
  // @@protoc_insertion_point(field_release:zetasql.AnnotationProto.value)
  _has_bits_[0] &= ~0x00000001u;
  ::zetasql::SimpleValueProto* temp = value_;
  value_ = nullptr;
  return temp;
}
inline ::zetasql::SimpleValueProto* AnnotationProto::_internal_mutable_value() {
  _has_bits_[0] |= 0x00000001u;
  if (value_ == nullptr) {
    auto* p = CreateMaybeMessage<::zetasql::SimpleValueProto>(GetArenaForAllocation());
    value_ = p;
  }
  return value_;
}
inline ::zetasql::SimpleValueProto* AnnotationProto::mutable_value() {
  ::zetasql::SimpleValueProto* _msg = _internal_mutable_value();
  // @@protoc_insertion_point(field_mutable:zetasql.AnnotationProto.value)
  return _msg;
}
inline void AnnotationProto::set_allocated_value(::zetasql::SimpleValueProto* value) {
  ::PROTOBUF_NAMESPACE_ID::Arena* message_arena = GetArenaForAllocation();
  if (message_arena == nullptr) {
    delete reinterpret_cast< ::PROTOBUF_NAMESPACE_ID::MessageLite*>(value_);
  }
  if (value) {
    ::PROTOBUF_NAMESPACE_ID::Arena* submessage_arena =
        ::PROTOBUF_NAMESPACE_ID::Arena::InternalHelper<
            ::PROTOBUF_NAMESPACE_ID::MessageLite>::GetOwningArena(
                reinterpret_cast<::PROTOBUF_NAMESPACE_ID::MessageLite*>(value));
    if (message_arena != submessage_arena) {
      value = ::PROTOBUF_NAMESPACE_ID::internal::GetOwnedMessage(
          message_arena, value, submessage_arena);
    }
    _has_bits_[0] |= 0x00000001u;
  } else {
    _has_bits_[0] &= ~0x00000001u;
  }
  value_ = value;
  // @@protoc_insertion_point(field_set_allocated:zetasql.AnnotationProto.value)
}

// -------------------------------------------------------------------

// AnnotationMapProto

// optional bool is_null = 4;
inline bool AnnotationMapProto::_internal_has_is_null() const {
  bool value = (_has_bits_[0] & 0x00000002u) != 0;
  return value;
}
inline bool AnnotationMapProto::has_is_null() const {
  return _internal_has_is_null();
}
inline void AnnotationMapProto::clear_is_null() {
  is_null_ = false;
  _has_bits_[0] &= ~0x00000002u;
}
inline bool AnnotationMapProto::_internal_is_null() const {
  return is_null_;
}
inline bool AnnotationMapProto::is_null() const {
  // @@protoc_insertion_point(field_get:zetasql.AnnotationMapProto.is_null)
  return _internal_is_null();
}
inline void AnnotationMapProto::_internal_set_is_null(bool value) {
  _has_bits_[0] |= 0x00000002u;
  is_null_ = value;
}
inline void AnnotationMapProto::set_is_null(bool value) {
  _internal_set_is_null(value);
  // @@protoc_insertion_point(field_set:zetasql.AnnotationMapProto.is_null)
}

// repeated .zetasql.AnnotationProto annotations = 1;
inline int AnnotationMapProto::_internal_annotations_size() const {
  return annotations_.size();
}
inline int AnnotationMapProto::annotations_size() const {
  return _internal_annotations_size();
}
inline void AnnotationMapProto::clear_annotations() {
  annotations_.Clear();
}
inline ::zetasql::AnnotationProto* AnnotationMapProto::mutable_annotations(int index) {
  // @@protoc_insertion_point(field_mutable:zetasql.AnnotationMapProto.annotations)
  return annotations_.Mutable(index);
}
inline ::PROTOBUF_NAMESPACE_ID::RepeatedPtrField< ::zetasql::AnnotationProto >*
AnnotationMapProto::mutable_annotations() {
  // @@protoc_insertion_point(field_mutable_list:zetasql.AnnotationMapProto.annotations)
  return &annotations_;
}
inline const ::zetasql::AnnotationProto& AnnotationMapProto::_internal_annotations(int index) const {
  return annotations_.Get(index);
}
inline const ::zetasql::AnnotationProto& AnnotationMapProto::annotations(int index) const {
  // @@protoc_insertion_point(field_get:zetasql.AnnotationMapProto.annotations)
  return _internal_annotations(index);
}
inline ::zetasql::AnnotationProto* AnnotationMapProto::_internal_add_annotations() {
  return annotations_.Add();
}
inline ::zetasql::AnnotationProto* AnnotationMapProto::add_annotations() {
  ::zetasql::AnnotationProto* _add = _internal_add_annotations();
  // @@protoc_insertion_point(field_add:zetasql.AnnotationMapProto.annotations)
  return _add;
}
inline const ::PROTOBUF_NAMESPACE_ID::RepeatedPtrField< ::zetasql::AnnotationProto >&
AnnotationMapProto::annotations() const {
  // @@protoc_insertion_point(field_list:zetasql.AnnotationMapProto.annotations)
  return annotations_;
}

// optional .zetasql.AnnotationMapProto array_element = 2;
inline bool AnnotationMapProto::_internal_has_array_element() const {
  bool value = (_has_bits_[0] & 0x00000001u) != 0;
  PROTOBUF_ASSUME(!value || array_element_ != nullptr);
  return value;
}
inline bool AnnotationMapProto::has_array_element() const {
  return _internal_has_array_element();
}
inline void AnnotationMapProto::clear_array_element() {
  if (array_element_ != nullptr) array_element_->Clear();
  _has_bits_[0] &= ~0x00000001u;
}
inline const ::zetasql::AnnotationMapProto& AnnotationMapProto::_internal_array_element() const {
  const ::zetasql::AnnotationMapProto* p = array_element_;
  return p != nullptr ? *p : reinterpret_cast<const ::zetasql::AnnotationMapProto&>(
      ::zetasql::_AnnotationMapProto_default_instance_);
}
inline const ::zetasql::AnnotationMapProto& AnnotationMapProto::array_element() const {
  // @@protoc_insertion_point(field_get:zetasql.AnnotationMapProto.array_element)
  return _internal_array_element();
}
inline void AnnotationMapProto::unsafe_arena_set_allocated_array_element(
    ::zetasql::AnnotationMapProto* array_element) {
  if (GetArenaForAllocation() == nullptr) {
    delete reinterpret_cast<::PROTOBUF_NAMESPACE_ID::MessageLite*>(array_element_);
  }
  array_element_ = array_element;
  if (array_element) {
    _has_bits_[0] |= 0x00000001u;
  } else {
    _has_bits_[0] &= ~0x00000001u;
  }
  // @@protoc_insertion_point(field_unsafe_arena_set_allocated:zetasql.AnnotationMapProto.array_element)
}
inline ::zetasql::AnnotationMapProto* AnnotationMapProto::release_array_element() {
  _has_bits_[0] &= ~0x00000001u;
  ::zetasql::AnnotationMapProto* temp = array_element_;
  array_element_ = nullptr;
#ifdef PROTOBUF_FORCE_COPY_IN_RELEASE
  auto* old =  reinterpret_cast<::PROTOBUF_NAMESPACE_ID::MessageLite*>(temp);
  temp = ::PROTOBUF_NAMESPACE_ID::internal::DuplicateIfNonNull(temp);
  if (GetArenaForAllocation() == nullptr) { delete old; }
#else  // PROTOBUF_FORCE_COPY_IN_RELEASE
  if (GetArenaForAllocation() != nullptr) {
    temp = ::PROTOBUF_NAMESPACE_ID::internal::DuplicateIfNonNull(temp);
  }
#endif  // !PROTOBUF_FORCE_COPY_IN_RELEASE
  return temp;
}
inline ::zetasql::AnnotationMapProto* AnnotationMapProto::unsafe_arena_release_array_element() {
  // @@protoc_insertion_point(field_release:zetasql.AnnotationMapProto.array_element)
  _has_bits_[0] &= ~0x00000001u;
  ::zetasql::AnnotationMapProto* temp = array_element_;
  array_element_ = nullptr;
  return temp;
}
inline ::zetasql::AnnotationMapProto* AnnotationMapProto::_internal_mutable_array_element() {
  _has_bits_[0] |= 0x00000001u;
  if (array_element_ == nullptr) {
    auto* p = CreateMaybeMessage<::zetasql::AnnotationMapProto>(GetArenaForAllocation());
    array_element_ = p;
  }
  return array_element_;
}
inline ::zetasql::AnnotationMapProto* AnnotationMapProto::mutable_array_element() {
  ::zetasql::AnnotationMapProto* _msg = _internal_mutable_array_element();
  // @@protoc_insertion_point(field_mutable:zetasql.AnnotationMapProto.array_element)
  return _msg;
}
inline void AnnotationMapProto::set_allocated_array_element(::zetasql::AnnotationMapProto* array_element) {
  ::PROTOBUF_NAMESPACE_ID::Arena* message_arena = GetArenaForAllocation();
  if (message_arena == nullptr) {
    delete array_element_;
  }
  if (array_element) {
    ::PROTOBUF_NAMESPACE_ID::Arena* submessage_arena =
        ::PROTOBUF_NAMESPACE_ID::Arena::InternalHelper<::zetasql::AnnotationMapProto>::GetOwningArena(array_element);
    if (message_arena != submessage_arena) {
      array_element = ::PROTOBUF_NAMESPACE_ID::internal::GetOwnedMessage(
          message_arena, array_element, submessage_arena);
    }
    _has_bits_[0] |= 0x00000001u;
  } else {
    _has_bits_[0] &= ~0x00000001u;
  }
  array_element_ = array_element;
  // @@protoc_insertion_point(field_set_allocated:zetasql.AnnotationMapProto.array_element)
}

// repeated .zetasql.AnnotationMapProto struct_fields = 3;
inline int AnnotationMapProto::_internal_struct_fields_size() const {
  return struct_fields_.size();
}
inline int AnnotationMapProto::struct_fields_size() const {
  return _internal_struct_fields_size();
}
inline void AnnotationMapProto::clear_struct_fields() {
  struct_fields_.Clear();
}
inline ::zetasql::AnnotationMapProto* AnnotationMapProto::mutable_struct_fields(int index) {
  // @@protoc_insertion_point(field_mutable:zetasql.AnnotationMapProto.struct_fields)
  return struct_fields_.Mutable(index);
}
inline ::PROTOBUF_NAMESPACE_ID::RepeatedPtrField< ::zetasql::AnnotationMapProto >*
AnnotationMapProto::mutable_struct_fields() {
  // @@protoc_insertion_point(field_mutable_list:zetasql.AnnotationMapProto.struct_fields)
  return &struct_fields_;
}
inline const ::zetasql::AnnotationMapProto& AnnotationMapProto::_internal_struct_fields(int index) const {
  return struct_fields_.Get(index);
}
inline const ::zetasql::AnnotationMapProto& AnnotationMapProto::struct_fields(int index) const {
  // @@protoc_insertion_point(field_get:zetasql.AnnotationMapProto.struct_fields)
  return _internal_struct_fields(index);
}
inline ::zetasql::AnnotationMapProto* AnnotationMapProto::_internal_add_struct_fields() {
  return struct_fields_.Add();
}
inline ::zetasql::AnnotationMapProto* AnnotationMapProto::add_struct_fields() {
  ::zetasql::AnnotationMapProto* _add = _internal_add_struct_fields();
  // @@protoc_insertion_point(field_add:zetasql.AnnotationMapProto.struct_fields)
  return _add;
}
inline const ::PROTOBUF_NAMESPACE_ID::RepeatedPtrField< ::zetasql::AnnotationMapProto >&
AnnotationMapProto::struct_fields() const {
  // @@protoc_insertion_point(field_list:zetasql.AnnotationMapProto.struct_fields)
  return struct_fields_;
}

#ifdef __GNUC__
  #pragma GCC diagnostic pop
#endif  // __GNUC__
// -------------------------------------------------------------------


// @@protoc_insertion_point(namespace_scope)

}  // namespace zetasql

// @@protoc_insertion_point(global_scope)

#include <google/protobuf/port_undef.inc>
#endif  // GOOGLE_PROTOBUF_INCLUDED_GOOGLE_PROTOBUF_INCLUDED_zetasql_2fpublic_2fannotation_2eproto
