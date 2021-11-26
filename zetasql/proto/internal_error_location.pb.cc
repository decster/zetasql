// Generated by the protocol buffer compiler.  DO NOT EDIT!
// source: zetasql/proto/internal_error_location.proto

#include "zetasql/proto/internal_error_location.pb.h"

#include <algorithm>

#include <google/protobuf/io/coded_stream.h>
#include <google/protobuf/extension_set.h>
#include <google/protobuf/wire_format_lite.h>
#include <google/protobuf/descriptor.h>
#include <google/protobuf/generated_message_reflection.h>
#include <google/protobuf/reflection_ops.h>
#include <google/protobuf/wire_format.h>
// @@protoc_insertion_point(includes)
#include <google/protobuf/port_def.inc>

PROTOBUF_PRAGMA_INIT_SEG
namespace zetasql {
constexpr InternalErrorLocation::InternalErrorLocation(
  ::PROTOBUF_NAMESPACE_ID::internal::ConstantInitialized)
  : error_source_()
  , filename_(&::PROTOBUF_NAMESPACE_ID::internal::fixed_address_empty_string)
  , byte_offset_(0){}
struct InternalErrorLocationDefaultTypeInternal {
  constexpr InternalErrorLocationDefaultTypeInternal()
    : _instance(::PROTOBUF_NAMESPACE_ID::internal::ConstantInitialized{}) {}
  ~InternalErrorLocationDefaultTypeInternal() {}
  union {
    InternalErrorLocation _instance;
  };
};
PROTOBUF_ATTRIBUTE_NO_DESTROY PROTOBUF_CONSTINIT InternalErrorLocationDefaultTypeInternal _InternalErrorLocation_default_instance_;
}  // namespace zetasql
static ::PROTOBUF_NAMESPACE_ID::Metadata file_level_metadata_zetasql_2fproto_2finternal_5ferror_5flocation_2eproto[1];
static constexpr ::PROTOBUF_NAMESPACE_ID::EnumDescriptor const** file_level_enum_descriptors_zetasql_2fproto_2finternal_5ferror_5flocation_2eproto = nullptr;
static constexpr ::PROTOBUF_NAMESPACE_ID::ServiceDescriptor const** file_level_service_descriptors_zetasql_2fproto_2finternal_5ferror_5flocation_2eproto = nullptr;

const uint32_t TableStruct_zetasql_2fproto_2finternal_5ferror_5flocation_2eproto::offsets[] PROTOBUF_SECTION_VARIABLE(protodesc_cold) = {
  PROTOBUF_FIELD_OFFSET(::zetasql::InternalErrorLocation, _has_bits_),
  PROTOBUF_FIELD_OFFSET(::zetasql::InternalErrorLocation, _internal_metadata_),
  ~0u,  // no _extensions_
  ~0u,  // no _oneof_case_
  ~0u,  // no _weak_field_map_
  ~0u,  // no _inlined_string_donated_
  PROTOBUF_FIELD_OFFSET(::zetasql::InternalErrorLocation, byte_offset_),
  PROTOBUF_FIELD_OFFSET(::zetasql::InternalErrorLocation, filename_),
  PROTOBUF_FIELD_OFFSET(::zetasql::InternalErrorLocation, error_source_),
  1,
  0,
  ~0u,
};
static const ::PROTOBUF_NAMESPACE_ID::internal::MigrationSchema schemas[] PROTOBUF_SECTION_VARIABLE(protodesc_cold) = {
  { 0, 9, -1, sizeof(::zetasql::InternalErrorLocation)},
};

static ::PROTOBUF_NAMESPACE_ID::Message const * const file_default_instances[] = {
  reinterpret_cast<const ::PROTOBUF_NAMESPACE_ID::Message*>(&::zetasql::_InternalErrorLocation_default_instance_),
};

const char descriptor_table_protodef_zetasql_2fproto_2finternal_5ferror_5flocation_2eproto[] PROTOBUF_SECTION_VARIABLE(protodesc_cold) =
  "\n+zetasql/proto/internal_error_location."
  "proto\022\007zetasql\032#zetasql/public/error_loc"
  "ation.proto\"j\n\025InternalErrorLocation\022\023\n\013"
  "byte_offset\030\003 \001(\005\022\020\n\010filename\030\004 \001(\t\022*\n\014e"
  "rror_source\030\005 \003(\0132\024.zetasql.ErrorSourceB"
  "1\n\022com.google.zetasqlB\033InternalErrorLoca"
  "tionProtos"
  ;
static const ::PROTOBUF_NAMESPACE_ID::internal::DescriptorTable*const descriptor_table_zetasql_2fproto_2finternal_5ferror_5flocation_2eproto_deps[1] = {
  &::descriptor_table_zetasql_2fpublic_2ferror_5flocation_2eproto,
};
static ::PROTOBUF_NAMESPACE_ID::internal::once_flag descriptor_table_zetasql_2fproto_2finternal_5ferror_5flocation_2eproto_once;
const ::PROTOBUF_NAMESPACE_ID::internal::DescriptorTable descriptor_table_zetasql_2fproto_2finternal_5ferror_5flocation_2eproto = {
  false, false, 250, descriptor_table_protodef_zetasql_2fproto_2finternal_5ferror_5flocation_2eproto, "zetasql/proto/internal_error_location.proto", 
  &descriptor_table_zetasql_2fproto_2finternal_5ferror_5flocation_2eproto_once, descriptor_table_zetasql_2fproto_2finternal_5ferror_5flocation_2eproto_deps, 1, 1,
  schemas, file_default_instances, TableStruct_zetasql_2fproto_2finternal_5ferror_5flocation_2eproto::offsets,
  file_level_metadata_zetasql_2fproto_2finternal_5ferror_5flocation_2eproto, file_level_enum_descriptors_zetasql_2fproto_2finternal_5ferror_5flocation_2eproto, file_level_service_descriptors_zetasql_2fproto_2finternal_5ferror_5flocation_2eproto,
};
PROTOBUF_ATTRIBUTE_WEAK const ::PROTOBUF_NAMESPACE_ID::internal::DescriptorTable* descriptor_table_zetasql_2fproto_2finternal_5ferror_5flocation_2eproto_getter() {
  return &descriptor_table_zetasql_2fproto_2finternal_5ferror_5flocation_2eproto;
}

// Force running AddDescriptors() at dynamic initialization time.
PROTOBUF_ATTRIBUTE_INIT_PRIORITY static ::PROTOBUF_NAMESPACE_ID::internal::AddDescriptorsRunner dynamic_init_dummy_zetasql_2fproto_2finternal_5ferror_5flocation_2eproto(&descriptor_table_zetasql_2fproto_2finternal_5ferror_5flocation_2eproto);
namespace zetasql {

// ===================================================================

class InternalErrorLocation::_Internal {
 public:
  using HasBits = decltype(std::declval<InternalErrorLocation>()._has_bits_);
  static void set_has_byte_offset(HasBits* has_bits) {
    (*has_bits)[0] |= 2u;
  }
  static void set_has_filename(HasBits* has_bits) {
    (*has_bits)[0] |= 1u;
  }
};

void InternalErrorLocation::clear_error_source() {
  error_source_.Clear();
}
InternalErrorLocation::InternalErrorLocation(::PROTOBUF_NAMESPACE_ID::Arena* arena,
                         bool is_message_owned)
  : ::PROTOBUF_NAMESPACE_ID::Message(arena, is_message_owned),
  error_source_(arena) {
  SharedCtor();
  if (!is_message_owned) {
    RegisterArenaDtor(arena);
  }
  // @@protoc_insertion_point(arena_constructor:zetasql.InternalErrorLocation)
}
InternalErrorLocation::InternalErrorLocation(const InternalErrorLocation& from)
  : ::PROTOBUF_NAMESPACE_ID::Message(),
      _has_bits_(from._has_bits_),
      error_source_(from.error_source_) {
  _internal_metadata_.MergeFrom<::PROTOBUF_NAMESPACE_ID::UnknownFieldSet>(from._internal_metadata_);
  filename_.UnsafeSetDefault(&::PROTOBUF_NAMESPACE_ID::internal::GetEmptyStringAlreadyInited());
  #ifdef PROTOBUF_FORCE_COPY_DEFAULT_STRING
    filename_.Set(&::PROTOBUF_NAMESPACE_ID::internal::GetEmptyStringAlreadyInited(), "", GetArenaForAllocation());
  #endif // PROTOBUF_FORCE_COPY_DEFAULT_STRING
  if (from._internal_has_filename()) {
    filename_.Set(::PROTOBUF_NAMESPACE_ID::internal::ArenaStringPtr::EmptyDefault{}, from._internal_filename(), 
      GetArenaForAllocation());
  }
  byte_offset_ = from.byte_offset_;
  // @@protoc_insertion_point(copy_constructor:zetasql.InternalErrorLocation)
}

inline void InternalErrorLocation::SharedCtor() {
filename_.UnsafeSetDefault(&::PROTOBUF_NAMESPACE_ID::internal::GetEmptyStringAlreadyInited());
#ifdef PROTOBUF_FORCE_COPY_DEFAULT_STRING
  filename_.Set(&::PROTOBUF_NAMESPACE_ID::internal::GetEmptyStringAlreadyInited(), "", GetArenaForAllocation());
#endif // PROTOBUF_FORCE_COPY_DEFAULT_STRING
byte_offset_ = 0;
}

InternalErrorLocation::~InternalErrorLocation() {
  // @@protoc_insertion_point(destructor:zetasql.InternalErrorLocation)
  if (GetArenaForAllocation() != nullptr) return;
  SharedDtor();
  _internal_metadata_.Delete<::PROTOBUF_NAMESPACE_ID::UnknownFieldSet>();
}

inline void InternalErrorLocation::SharedDtor() {
  GOOGLE_DCHECK(GetArenaForAllocation() == nullptr);
  filename_.DestroyNoArena(&::PROTOBUF_NAMESPACE_ID::internal::GetEmptyStringAlreadyInited());
}

void InternalErrorLocation::ArenaDtor(void* object) {
  InternalErrorLocation* _this = reinterpret_cast< InternalErrorLocation* >(object);
  (void)_this;
}
void InternalErrorLocation::RegisterArenaDtor(::PROTOBUF_NAMESPACE_ID::Arena*) {
}
void InternalErrorLocation::SetCachedSize(int size) const {
  _cached_size_.Set(size);
}

void InternalErrorLocation::Clear() {
// @@protoc_insertion_point(message_clear_start:zetasql.InternalErrorLocation)
  uint32_t cached_has_bits = 0;
  // Prevent compiler warnings about cached_has_bits being unused
  (void) cached_has_bits;

  error_source_.Clear();
  cached_has_bits = _has_bits_[0];
  if (cached_has_bits & 0x00000001u) {
    filename_.ClearNonDefaultToEmpty();
  }
  byte_offset_ = 0;
  _has_bits_.Clear();
  _internal_metadata_.Clear<::PROTOBUF_NAMESPACE_ID::UnknownFieldSet>();
}

const char* InternalErrorLocation::_InternalParse(const char* ptr, ::PROTOBUF_NAMESPACE_ID::internal::ParseContext* ctx) {
#define CHK_(x) if (PROTOBUF_PREDICT_FALSE(!(x))) goto failure
  _Internal::HasBits has_bits{};
  while (!ctx->Done(&ptr)) {
    uint32_t tag;
    ptr = ::PROTOBUF_NAMESPACE_ID::internal::ReadTag(ptr, &tag);
    switch (tag >> 3) {
      // optional int32 byte_offset = 3;
      case 3:
        if (PROTOBUF_PREDICT_TRUE(static_cast<uint8_t>(tag) == 24)) {
          _Internal::set_has_byte_offset(&has_bits);
          byte_offset_ = ::PROTOBUF_NAMESPACE_ID::internal::ReadVarint32(&ptr);
          CHK_(ptr);
        } else
          goto handle_unusual;
        continue;
      // optional string filename = 4;
      case 4:
        if (PROTOBUF_PREDICT_TRUE(static_cast<uint8_t>(tag) == 34)) {
          auto str = _internal_mutable_filename();
          ptr = ::PROTOBUF_NAMESPACE_ID::internal::InlineGreedyStringParser(str, ptr, ctx);
          #ifndef NDEBUG
          ::PROTOBUF_NAMESPACE_ID::internal::VerifyUTF8(str, "zetasql.InternalErrorLocation.filename");
          #endif  // !NDEBUG
          CHK_(ptr);
        } else
          goto handle_unusual;
        continue;
      // repeated .zetasql.ErrorSource error_source = 5;
      case 5:
        if (PROTOBUF_PREDICT_TRUE(static_cast<uint8_t>(tag) == 42)) {
          ptr -= 1;
          do {
            ptr += 1;
            ptr = ctx->ParseMessage(_internal_add_error_source(), ptr);
            CHK_(ptr);
            if (!ctx->DataAvailable(ptr)) break;
          } while (::PROTOBUF_NAMESPACE_ID::internal::ExpectTag<42>(ptr));
        } else
          goto handle_unusual;
        continue;
      default:
        goto handle_unusual;
    }  // switch
  handle_unusual:
    if ((tag == 0) || ((tag & 7) == 4)) {
      CHK_(ptr);
      ctx->SetLastTag(tag);
      goto message_done;
    }
    ptr = UnknownFieldParse(
        tag,
        _internal_metadata_.mutable_unknown_fields<::PROTOBUF_NAMESPACE_ID::UnknownFieldSet>(),
        ptr, ctx);
    CHK_(ptr != nullptr);
  }  // while
message_done:
  _has_bits_.Or(has_bits);
  return ptr;
failure:
  ptr = nullptr;
  goto message_done;
#undef CHK_
}

uint8_t* InternalErrorLocation::_InternalSerialize(
    uint8_t* target, ::PROTOBUF_NAMESPACE_ID::io::EpsCopyOutputStream* stream) const {
  // @@protoc_insertion_point(serialize_to_array_start:zetasql.InternalErrorLocation)
  uint32_t cached_has_bits = 0;
  (void) cached_has_bits;

  cached_has_bits = _has_bits_[0];
  // optional int32 byte_offset = 3;
  if (cached_has_bits & 0x00000002u) {
    target = stream->EnsureSpace(target);
    target = ::PROTOBUF_NAMESPACE_ID::internal::WireFormatLite::WriteInt32ToArray(3, this->_internal_byte_offset(), target);
  }

  // optional string filename = 4;
  if (cached_has_bits & 0x00000001u) {
    ::PROTOBUF_NAMESPACE_ID::internal::WireFormat::VerifyUTF8StringNamedField(
      this->_internal_filename().data(), static_cast<int>(this->_internal_filename().length()),
      ::PROTOBUF_NAMESPACE_ID::internal::WireFormat::SERIALIZE,
      "zetasql.InternalErrorLocation.filename");
    target = stream->WriteStringMaybeAliased(
        4, this->_internal_filename(), target);
  }

  // repeated .zetasql.ErrorSource error_source = 5;
  for (unsigned int i = 0,
      n = static_cast<unsigned int>(this->_internal_error_source_size()); i < n; i++) {
    target = stream->EnsureSpace(target);
    target = ::PROTOBUF_NAMESPACE_ID::internal::WireFormatLite::
      InternalWriteMessage(5, this->_internal_error_source(i), target, stream);
  }

  if (PROTOBUF_PREDICT_FALSE(_internal_metadata_.have_unknown_fields())) {
    target = ::PROTOBUF_NAMESPACE_ID::internal::WireFormat::InternalSerializeUnknownFieldsToArray(
        _internal_metadata_.unknown_fields<::PROTOBUF_NAMESPACE_ID::UnknownFieldSet>(::PROTOBUF_NAMESPACE_ID::UnknownFieldSet::default_instance), target, stream);
  }
  // @@protoc_insertion_point(serialize_to_array_end:zetasql.InternalErrorLocation)
  return target;
}

size_t InternalErrorLocation::ByteSizeLong() const {
// @@protoc_insertion_point(message_byte_size_start:zetasql.InternalErrorLocation)
  size_t total_size = 0;

  uint32_t cached_has_bits = 0;
  // Prevent compiler warnings about cached_has_bits being unused
  (void) cached_has_bits;

  // repeated .zetasql.ErrorSource error_source = 5;
  total_size += 1UL * this->_internal_error_source_size();
  for (const auto& msg : this->error_source_) {
    total_size +=
      ::PROTOBUF_NAMESPACE_ID::internal::WireFormatLite::MessageSize(msg);
  }

  cached_has_bits = _has_bits_[0];
  if (cached_has_bits & 0x00000003u) {
    // optional string filename = 4;
    if (cached_has_bits & 0x00000001u) {
      total_size += 1 +
        ::PROTOBUF_NAMESPACE_ID::internal::WireFormatLite::StringSize(
          this->_internal_filename());
    }

    // optional int32 byte_offset = 3;
    if (cached_has_bits & 0x00000002u) {
      total_size += ::PROTOBUF_NAMESPACE_ID::internal::WireFormatLite::Int32SizePlusOne(this->_internal_byte_offset());
    }

  }
  return MaybeComputeUnknownFieldsSize(total_size, &_cached_size_);
}

const ::PROTOBUF_NAMESPACE_ID::Message::ClassData InternalErrorLocation::_class_data_ = {
    ::PROTOBUF_NAMESPACE_ID::Message::CopyWithSizeCheck,
    InternalErrorLocation::MergeImpl
};
const ::PROTOBUF_NAMESPACE_ID::Message::ClassData*InternalErrorLocation::GetClassData() const { return &_class_data_; }

void InternalErrorLocation::MergeImpl(::PROTOBUF_NAMESPACE_ID::Message* to,
                      const ::PROTOBUF_NAMESPACE_ID::Message& from) {
  static_cast<InternalErrorLocation *>(to)->MergeFrom(
      static_cast<const InternalErrorLocation &>(from));
}


void InternalErrorLocation::MergeFrom(const InternalErrorLocation& from) {
// @@protoc_insertion_point(class_specific_merge_from_start:zetasql.InternalErrorLocation)
  GOOGLE_DCHECK_NE(&from, this);
  uint32_t cached_has_bits = 0;
  (void) cached_has_bits;

  error_source_.MergeFrom(from.error_source_);
  cached_has_bits = from._has_bits_[0];
  if (cached_has_bits & 0x00000003u) {
    if (cached_has_bits & 0x00000001u) {
      _internal_set_filename(from._internal_filename());
    }
    if (cached_has_bits & 0x00000002u) {
      byte_offset_ = from.byte_offset_;
    }
    _has_bits_[0] |= cached_has_bits;
  }
  _internal_metadata_.MergeFrom<::PROTOBUF_NAMESPACE_ID::UnknownFieldSet>(from._internal_metadata_);
}

void InternalErrorLocation::CopyFrom(const InternalErrorLocation& from) {
// @@protoc_insertion_point(class_specific_copy_from_start:zetasql.InternalErrorLocation)
  if (&from == this) return;
  Clear();
  MergeFrom(from);
}

bool InternalErrorLocation::IsInitialized() const {
  return true;
}

void InternalErrorLocation::InternalSwap(InternalErrorLocation* other) {
  using std::swap;
  auto* lhs_arena = GetArenaForAllocation();
  auto* rhs_arena = other->GetArenaForAllocation();
  _internal_metadata_.InternalSwap(&other->_internal_metadata_);
  swap(_has_bits_[0], other->_has_bits_[0]);
  error_source_.InternalSwap(&other->error_source_);
  ::PROTOBUF_NAMESPACE_ID::internal::ArenaStringPtr::InternalSwap(
      &::PROTOBUF_NAMESPACE_ID::internal::GetEmptyStringAlreadyInited(),
      &filename_, lhs_arena,
      &other->filename_, rhs_arena
  );
  swap(byte_offset_, other->byte_offset_);
}

::PROTOBUF_NAMESPACE_ID::Metadata InternalErrorLocation::GetMetadata() const {
  return ::PROTOBUF_NAMESPACE_ID::internal::AssignDescriptors(
      &descriptor_table_zetasql_2fproto_2finternal_5ferror_5flocation_2eproto_getter, &descriptor_table_zetasql_2fproto_2finternal_5ferror_5flocation_2eproto_once,
      file_level_metadata_zetasql_2fproto_2finternal_5ferror_5flocation_2eproto[0]);
}

// @@protoc_insertion_point(namespace_scope)
}  // namespace zetasql
PROTOBUF_NAMESPACE_OPEN
template<> PROTOBUF_NOINLINE ::zetasql::InternalErrorLocation* Arena::CreateMaybeMessage< ::zetasql::InternalErrorLocation >(Arena* arena) {
  return Arena::CreateMessageInternal< ::zetasql::InternalErrorLocation >(arena);
}
PROTOBUF_NAMESPACE_CLOSE

// @@protoc_insertion_point(global_scope)
#include <google/protobuf/port_undef.inc>