//
// Copyright 2019 Google LLC
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

#include "zetasql/public/functions/cast_date_time.h"

#include "zetasql/base/testing/status_matchers.h"
#include "zetasql/compliance/functions_testlib.h"
#include "zetasql/public/type.pb.h"
#include "zetasql/public/value.h"
#include "zetasql/testing/test_function.h"
#include "absl/strings/substitute.h"

namespace zetasql {
namespace functions {
namespace {

using testing::HasSubstr;
using zetasql_base::testing::StatusIs;

using cast_date_time_internal::FormatElement;
using cast_date_time_internal::FormatElementType;
using cast_date_time_internal::GetFormatElements;

static void ExecuteFormatElementParsingTest(
    absl::string_view format_str,
    const std::vector<FormatElement>& expected_format_elements,
    std::string error_message) {
  std::string upper_format_str_temp = absl::AsciiStrToUpper(format_str);

  if (error_message.empty()) {
    auto status_or_format_elements = GetFormatElements(format_str);
    ZETASQL_EXPECT_OK(status_or_format_elements);

    std::vector<FormatElement>& format_elements =
        status_or_format_elements.value();

    EXPECT_EQ(format_elements.size(), expected_format_elements.size());
    for (size_t i = 0; i < format_elements.size(); ++i) {
      EXPECT_EQ(format_elements[i].type, expected_format_elements[i].type);
      EXPECT_EQ(format_elements[i].original_str,
                expected_format_elements[i].original_str);
    }
  } else {
    auto status_or_format_elements = GetFormatElements(format_str);
    EXPECT_THAT(
        status_or_format_elements.status(),
        StatusIs(absl::StatusCode::kOutOfRange, HasSubstr(error_message)));
  }
}

static std::vector<FormatElement> GetAllSupportedFormatElements() {
  std::vector<FormatElement> all_format_elements;
  /*Literals*/
  all_format_elements.push_back(
      FormatElement(FormatElementType::kLiteral, "-"));
  all_format_elements.push_back(
      FormatElement(FormatElementType::kLiteral, "."));
  all_format_elements.push_back(
      FormatElement(FormatElementType::kLiteral, "/"));
  all_format_elements.push_back(
      FormatElement(FormatElementType::kLiteral, ","));
  all_format_elements.push_back(
      FormatElement(FormatElementType::kLiteral, "'"));
  all_format_elements.push_back(
      FormatElement(FormatElementType::kLiteral, ";"));
  all_format_elements.push_back(
      FormatElement(FormatElementType::kLiteral, ":"));
  // the first output character of Text format element could be a number
  all_format_elements.push_back(
      FormatElement(FormatElementType::kDoubleQuotedLiteral, "abc"));
  all_format_elements.push_back(
      FormatElement(FormatElementType::kWhitespace, " "));

  /*Year*/
  all_format_elements.push_back(
      FormatElement(FormatElementType::kYear, "YYYY"));
  all_format_elements.push_back(
      FormatElement(FormatElementType::kYear, "RRRR"));
  all_format_elements.push_back(FormatElement(FormatElementType::kYear, "YYY"));
  all_format_elements.push_back(FormatElement(FormatElementType::kYear, "RR"));
  all_format_elements.push_back(FormatElement(FormatElementType::kYear, "YY"));
  all_format_elements.push_back(
      FormatElement(FormatElementType::kYear, "SYEAR"));
  all_format_elements.push_back(
      FormatElement(FormatElementType::kYear, "Y,YYY"));
  all_format_elements.push_back(FormatElement(FormatElementType::kYear, "Y"));
  all_format_elements.push_back(FormatElement(FormatElementType::kYear, "IYY"));
  all_format_elements.push_back(FormatElement(FormatElementType::kYear, "IY"));
  all_format_elements.push_back(FormatElement(FormatElementType::kYear, "I"));
  all_format_elements.push_back(
      FormatElement(FormatElementType::kYear, "SYYYY"));
  all_format_elements.push_back(
      FormatElement(FormatElementType::kYear, "YEAR"));

  /*Month*/
  all_format_elements.push_back(FormatElement(FormatElementType::kMonth, "MM"));
  all_format_elements.push_back(
      FormatElement(FormatElementType::kMonth, "MON"));
  all_format_elements.push_back(
      FormatElement(FormatElementType::kMonth, "MONTH"));
  all_format_elements.push_back(FormatElement(FormatElementType::kMonth, "RM"));

  /*Day*/
  all_format_elements.push_back(FormatElement(FormatElementType::kDay, "DDD"));
  all_format_elements.push_back(FormatElement(FormatElementType::kDay, "DAY"));
  all_format_elements.push_back(FormatElement(FormatElementType::kDay, "DD"));
  all_format_elements.push_back(FormatElement(FormatElementType::kDay, "J"));
  all_format_elements.push_back(FormatElement(FormatElementType::kDay, "DY"));
  all_format_elements.push_back(FormatElement(FormatElementType::kDay, "D"));

  /*Hour*/
  all_format_elements.push_back(FormatElement(FormatElementType::kHour, "HH"));
  all_format_elements.push_back(
      FormatElement(FormatElementType::kHour, "HH12"));
  all_format_elements.push_back(
      FormatElement(FormatElementType::kHour, "HH24"));

  /*Minute*/
  all_format_elements.push_back(
      FormatElement(FormatElementType::kMinute, "MI"));

  /*Second*/
  all_format_elements.push_back(
      FormatElement(FormatElementType::kSecond, "SSSSS"));
  all_format_elements.push_back(
      FormatElement(FormatElementType::kSecond, "SS"));
  // FF1~FF9
  for (int i = 1; i < 10; ++i) {
    all_format_elements.push_back(
        FormatElement(FormatElementType::kSecond, absl::StrCat("FF", i)));
  }

  /*Meridian indicator*/
  all_format_elements.push_back(
      FormatElement(FormatElementType::kMeridianIndicator, "A.M."));
  all_format_elements.push_back(
      FormatElement(FormatElementType::kMeridianIndicator, "AM"));
  all_format_elements.push_back(
      FormatElement(FormatElementType::kMeridianIndicator, "P.M."));
  all_format_elements.push_back(
      FormatElement(FormatElementType::kMeridianIndicator, "PM"));

  /*Time zone*/
  all_format_elements.push_back(
      FormatElement(FormatElementType::kTimeZone, "TZH"));
  all_format_elements.push_back(
      FormatElement(FormatElementType::kTimeZone, "TZM"));

  /*Century*/
  all_format_elements.push_back(
      FormatElement(FormatElementType::kCentury, "CC"));
  all_format_elements.push_back(
      FormatElement(FormatElementType::kCentury, "SCC"));

  /*Quarter*/
  all_format_elements.push_back(
      FormatElement(FormatElementType::kQuarter, "Q"));

  /*Week*/
  all_format_elements.push_back(FormatElement(FormatElementType::kWeek, "IW"));
  all_format_elements.push_back(FormatElement(FormatElementType::kWeek, "WW"));
  all_format_elements.push_back(FormatElement(FormatElementType::kWeek, "W"));

  /*Era Indicator*/
  all_format_elements.push_back(
      FormatElement(FormatElementType::kEraIndicator, "AD"));
  all_format_elements.push_back(
      FormatElement(FormatElementType::kEraIndicator, "BC"));
  all_format_elements.push_back(
      FormatElement(FormatElementType::kEraIndicator, "A.D."));
  all_format_elements.push_back(
      FormatElement(FormatElementType::kEraIndicator, "B.C."));

  /*Misc*/
  all_format_elements.push_back(FormatElement(FormatElementType::kMisc, "SP"));
  all_format_elements.push_back(FormatElement(FormatElementType::kMisc, "FM"));
  all_format_elements.push_back(
      FormatElement(FormatElementType::kMisc, "SPTH"));
  all_format_elements.push_back(FormatElement(FormatElementType::kMisc, "TH"));
  all_format_elements.push_back(
      FormatElement(FormatElementType::kMisc, "THSP"));

  return all_format_elements;
}

static void TestCastStringToTimestamp(const FunctionTestCall& test) {
  // Ignore tests that do not have the time zone explicitly specified since
  // the function library requires a timezone.  Also ignore test cases
  // with NULL value inputs.  The date/time function library is only
  // implemented for non-NULL values.
  const int expected_param_size = 4;
  if (test.params.params().size() != expected_param_size) {
    return;
  }

  for (size_t i = 0; i < expected_param_size; ++i) {
    if (test.params.param(i).is_null()) {
      return;
    }
  }

  const Value& format_param = test.params.param(0);
  const Value& timestamp_string_param = test.params.param(1);
  const Value& timezone_param = test.params.param(2);
  const Value& current_timestamp_param = test.params.param(3);
  int64_t result_timestamp;
  absl::Time base_time_result;
  std::string test_string;
  absl::Status status;
  absl::Status base_time_status;

  status = CastStringToTimestamp(
      format_param.string_value(), timestamp_string_param.string_value(),
      timezone_param.string_value(), current_timestamp_param.ToTime(),
      &result_timestamp);
  base_time_status = CastStringToTimestamp(
      format_param.string_value(), timestamp_string_param.string_value(),
      timezone_param.string_value(), current_timestamp_param.ToTime(),
      &base_time_result);
  test_string = absl::Substitute(
      absl::StrCat(test.function_name, "($0, $1, $2, $3)"),
      format_param.DebugString(), timestamp_string_param.DebugString(),
      timezone_param.DebugString(), current_timestamp_param.DebugString());

  const QueryParamsWithResult::Result& micros_test_result = zetasql_base::FindOrDie(
      test.params.results(), QueryParamsWithResult::kEmptyFeatureSet);
  if (micros_test_result.status.ok()) {
    ZETASQL_EXPECT_OK(status) << test_string;
    if (status.ok()) {
      EXPECT_EQ(TYPE_TIMESTAMP, micros_test_result.result.type_kind())
          << test_string;
      EXPECT_EQ(micros_test_result.result.ToTime(),
                absl::FromUnixMicros(result_timestamp))
          << test_string << "\nexpected: "
          << absl::FormatTime(
                 absl::FromUnixMicros(micros_test_result.result.ToUnixMicros()))
          << "\nactual: "
          << absl::FormatTime(absl::FromUnixMicros(result_timestamp));
    }
  } else {
    EXPECT_FALSE(status.ok())
        << test_string << "\nstatus: " << micros_test_result.status;
  }

  const std::set<LanguageFeature> feature_set{FEATURE_TIMESTAMP_NANOS};
  const QueryParamsWithResult::Result& nanos_test_result =
      zetasql_base::FindOrDie(test.params.results(), feature_set);

  if (nanos_test_result.status.ok()) {
    ZETASQL_EXPECT_OK(base_time_status) << test_string;
    EXPECT_EQ(TYPE_TIMESTAMP, nanos_test_result.result.type_kind())
        << test_string;
    EXPECT_EQ(nanos_test_result.result.ToTime(), base_time_result)
        << test_string << ": "
        << absl::FormatTime(nanos_test_result.result.ToTime()) << " vs "
        << absl::FormatTime(base_time_result);
  } else {
    EXPECT_FALSE(base_time_status.ok())
        << test_string << "\nstatus: " << nanos_test_result.status;
  }
}

static void TestValidateFormatStringForParsing(
    zetasql::TypeKind out_type, absl::string_view format_string,
    std::string expected_error_message,
    absl::StatusCode expected_status_code = absl::StatusCode::kOutOfRange) {
  absl::Status res = ValidateFormatStringForParsing(format_string, out_type);

  if (expected_error_message.empty()) {
    ZETASQL_EXPECT_OK(res);
  } else {
    EXPECT_THAT(
        res, StatusIs(expected_status_code, HasSubstr(expected_error_message)));
  }
}

static void TestValidateFormatStringForFormatting(
    zetasql::TypeKind out_type, absl::string_view format_string,
    std::string expected_error_message,
    absl::StatusCode expected_status_code = absl::StatusCode::kOutOfRange) {
  absl::Status res = ValidateFormatStringForFormatting(format_string, out_type);

  if (expected_error_message.empty()) {
    ZETASQL_EXPECT_OK(res);
  } else {
    EXPECT_THAT(
        res, StatusIs(expected_status_code, HasSubstr(expected_error_message)));
  }
}

static void TestCastFormatFunction(
    const FunctionTestCall& testcase,
    const std::function<absl::Status(std::string* result_string,
                                     std::string* test_name)>&
        function_to_test) {
  std::string test_name;
  std::string result_string;
  const auto result_status = function_to_test(&result_string, &test_name);
  if (result_status.ok()) {
    EXPECT_EQ(TYPE_STRING, testcase.params.result().type_kind()) << test_name;
    EXPECT_EQ(result_string, testcase.params.result().string_value())
        << test_name;
  } else {
    EXPECT_FALSE(result_status.ok()) << test_name;
  }
}

static void TestCastFormatDatetime(const FunctionTestCall& testcase) {
  auto FormatDatetimeFunc = [&testcase](std::string* result_string,
                                        std::string* test_name) {
    ZETASQL_DCHECK_EQ(testcase.params.num_params(), 2);
    const Value& format_param = testcase.params.param(1);
    const Value& datetime_param = testcase.params.param(0);
    *test_name = absl::Substitute(
        absl::StrCat(testcase.function_name, "($0, $1)"),
        format_param.DebugString(), datetime_param.DebugString());
    return CastFormatDatetimeToString(format_param.string_value(),
                                      datetime_param.datetime_value(),
                                      result_string);
  };
  TestCastFormatFunction(testcase, FormatDatetimeFunc);
}

static void TestCastFormatTime(const FunctionTestCall& testcase) {
  auto FormatTimeFunc = [&testcase](std::string* result_string,
                                    std::string* test_name) {
    ZETASQL_DCHECK_EQ(testcase.params.num_params(), 2);
    const Value& format_param = testcase.params.param(1);
    const Value& time_param = testcase.params.param(0);
    *test_name =
        absl::Substitute(absl::StrCat(testcase.function_name, "($0, $1)"),
                         format_param.DebugString(), time_param.DebugString());
    return CastFormatTimeToString(format_param.string_value(),
                                  time_param.time_value(),
                                  result_string);
  };
  TestCastFormatFunction(testcase, FormatTimeFunc);
}

static void TestCastFormatDate(const FunctionTestCall& testcase) {
  auto FormatDateFunc = [&testcase](std::string* result_string,
                                    std::string* test_name) {
    ZETASQL_DCHECK_EQ(testcase.params.num_params(), 2);
    const Value& format_param = testcase.params.param(1);
    const Value& date_param = testcase.params.param(0);
    *test_name =
        absl::Substitute(absl::StrCat(testcase.function_name, "($0, $1)"),
                         format_param.DebugString(), date_param.DebugString());
    return CastFormatDateToString(format_param.string_value(),
                                  date_param.date_value(),
                                  result_string);
  };
  TestCastFormatFunction(testcase, FormatDateFunc);
}

static void TestCastFormatTimestamp(const FunctionTestCall& testcase) {
  auto FormatTimestampFunc = [&testcase](std::string* result_string,
                                         std::string* test_name) {
    ZETASQL_DCHECK_EQ(testcase.params.num_params(), 3);
    const Value& format_param = testcase.params.param(1);
    const Value& timestamp_param = testcase.params.param(0);
    const Value& timezone_param = testcase.params.param(2);
    *test_name = absl::Substitute(
        absl::StrCat(testcase.function_name, "($0, $1, $2)"),
        format_param.DebugString(), timestamp_param.DebugString(),
        timezone_param.DebugString());
    return CastFormatTimestampToString(
        format_param.string_value(),
        timestamp_param.timestamp_value(),
        timezone_param.string_value(), result_string);
  };
  TestCastFormatFunction(testcase, FormatTimestampFunc);
}

static void CastStringToDateTimestampFunctionTest(
    const FunctionTestCall& test) {
  if (test.function_name == "cast_string_to_timestamp") {
    TestCastStringToTimestamp(test);
  } else {
    ASSERT_FALSE(true) << "Test cases do not support function: "
                       << test.function_name;
  }
}

class CastStringToDateTimeTestWithParam
    : public ::testing::TestWithParam<FunctionTestCall> {};

TEST_P(CastStringToDateTimeTestWithParam, CastStringToDateTimestampTests) {
  const FunctionTestCall& test = GetParam();
  CastStringToDateTimestampFunctionTest(test);
}

// These tests are populated in zetasql/compliance/functions_testlib.cc.
INSTANTIATE_TEST_SUITE_P(
    CastStringToDateTimestampTests, CastStringToDateTimeTestWithParam,
    testing::ValuesIn(GetFunctionTestsCastStringToDateTimestamp()));

TEST(StringToTimestampTests, ValidateFormatStringForParsing) {
  TestValidateFormatStringForParsing(TYPE_TIMESTAMP, "", "");
  TestValidateFormatStringForParsing(TYPE_TIMESTAMP, "£\xff£",
                                     "Input string is not valid UTF-8");
  TestValidateFormatStringForParsing(TYPE_TIMESTAMP, "invalid_element",
                                     "Cannot find matched format element");
  TestValidateFormatStringForParsing(
      TYPE_TIMESTAMP, "DAY",
      "Format Element 'DAY' is not supported for parsing");
  TestValidateFormatStringForParsing(
      TYPE_TIMESTAMP, "MiMI",
      "More than one format element in upper case form MI exist: 'Mi', 'MI'");
  TestValidateFormatStringForParsing(
      TYPE_TIMESTAMP, "fF2Ff2",
      "More than one format element in upper case form "
      "FF2 exist: 'fF2', 'Ff2'");
  // The single occurrence limit of distinct uppercase forms only applies to
  // non-literal format elements.
  TestValidateFormatStringForParsing(TYPE_TIMESTAMP, R"(Mi"MI")", "");
  TestValidateFormatStringForParsing(TYPE_TIMESTAMP, "- -", "");
  TestValidateFormatStringForParsing(
      TYPE_TIMESTAMP, "YYYYRR",
      "More than one format element of type YEAR exist: 'YYYY' and 'RR'");
  TestValidateFormatStringForParsing(
      TYPE_TIMESTAMP, "MonthMM",
      "More than one format element of type MONTH exist: 'Month' and 'MM'");
  TestValidateFormatStringForParsing(
      TYPE_TIMESTAMP, "DDDDd",
      "More than one format element of type DAY exist: 'DDD' and 'Dd'");
  TestValidateFormatStringForParsing(
      TYPE_TIMESTAMP, "HH24HH",
      "More than one format element of type HOUR exist: 'HH24' and 'HH'");
  TestValidateFormatStringForParsing(
      TYPE_TIMESTAMP, "HH24hh12",
      "More than one format element of type HOUR exist: 'HH24' and 'hh12'");
  TestValidateFormatStringForParsing(
      TYPE_TIMESTAMP, "MonDDD",
      "Format element of type MONTH ('Mon') and format "
      "element 'DDD' cannot exist simultaneously");
  TestValidateFormatStringForParsing(
      TYPE_TIMESTAMP, "HH24A.M.",
      "Format element of type MERIDIAN_INDICATOR ('A.M.') and format "
      "element 'HH24' cannot exist simultaneously");
  TestValidateFormatStringForParsing(
      TYPE_TIMESTAMP, "hh",
      "Format element of type MERIDIAN_INDICATOR is required when format "
      "element 'hh' exists");
  TestValidateFormatStringForParsing(
      TYPE_TIMESTAMP, "Hh12",
      "Format element of type MERIDIAN_INDICATOR is required when format "
      "element 'Hh12' exists");
  TestValidateFormatStringForParsing(
      TYPE_TIMESTAMP, "AM",
      "Format element in the form of 'HH'/'HH12' is required when format "
      "element of type MERIDIAN_INDICATOR ('AM') exists");
  TestValidateFormatStringForParsing(
      TYPE_TIMESTAMP, "SSSSSHH12AM",
      "Format element of type HOUR ('HH12') and format "
      "element 'SSSSS' cannot exist simultaneously");
  TestValidateFormatStringForParsing(
      TYPE_TIMESTAMP, "SSSSSMi",
      "Format element of type MINUTE ('Mi') and format "
      "element 'SSSSS' cannot exist simultaneously");
  TestValidateFormatStringForParsing(
      TYPE_TIMESTAMP, "SsYYSSSSS",
      "Format elements 'SSSSS' and 'Ss' cannot exist simultaneously");
  TestValidateFormatStringForParsing(TYPE_INT64, "",
                                     "Unsupported output type for validation",
                                     absl::StatusCode::kInvalidArgument);
}

// Returns the list of format elements that are valid for TIME, i.e. hour,
// minute, second, fractional second, AM/PM.
std::vector<std::string> GetTimeElements() {
  std::vector<std::string> elements = {
      "HH",  "HH12", "HH24", "MI",  "SS",  "SSSSS", "FF1",  "FF2", "FF3", "FF4",
      "FF5", "FF6",  "FF7",  "FF8", "FF9", "AM",    "A.M.", "PM",  "P.M."};
  return elements;
}

// Returns the list of format elements that are valid for DATE, i.e. year,
// month, day
std::vector<std::string> GetDateElements() {
  std::vector<std::string> elements = {
      "YYYY", "YYY",   "YY", "YY",  "Y",   "RRRR", "RR", "MM",
      "MON",  "MONTH", "DD", "DDD", "DAY", "J",    "DY", "D"};
  return elements;
}

// Returns the list of time zone format element.
std::vector<std::string> GetTimeZoneElements() {
  std::vector<std::string> elements = {
    "TZH",  "TZM",
  };
  return elements;
}

TEST(DateAndTimeToStringTests, ValidateFormatStringForFormattingDate) {
  // Success case
  for (const auto& element : GetDateElements()) {
    TestValidateFormatStringForFormatting(TYPE_DATE, element, "");
  }

  // Time elements are not allowed for DATE
  for (const auto& element : GetTimeElements()) {
    TestValidateFormatStringForFormatting(
        TYPE_DATE, element,
        absl::Substitute("DATE does not support $0", element));
  }

  // Time zone elements are not allowed for DATE
  for (const auto& element : GetTimeZoneElements()) {
    TestValidateFormatStringForFormatting(
        TYPE_DATE, element,
        absl::Substitute("DATE does not support $0", element));
  }

  // Format string is not valid utf-8 string
  TestValidateFormatStringForFormatting(
      TYPE_DATE, "\xFF\xFF", "Format string is not a valid UTF-8 string");

  // Invalid format element
  TestValidateFormatStringForFormatting(
      TYPE_DATE, "a", "Cannot find matched format element at 0");
}

TEST(DateAndTimeToStringTests, ValidateFormatStringForFormattingDateTime) {
  // Success case
  for (const auto& element : GetDateElements()) {
    TestValidateFormatStringForFormatting(TYPE_DATETIME, element, "");
  }
  for (const auto& element : GetTimeElements()) {
    TestValidateFormatStringForFormatting(TYPE_DATETIME, element, "");
  }

  // Time zone elements are not allowed for DATETIME
  for (const auto& element : GetTimeZoneElements()) {
    TestValidateFormatStringForFormatting(
        TYPE_DATETIME, element,
        absl::Substitute("DATETIME does not support $0", element));
  }

  // Format string is not valid utf-8 string
  TestValidateFormatStringForFormatting(
      TYPE_DATETIME, "\xFF\xFF", "Format string is not a valid UTF-8 string");

  // Invalid format element
  TestValidateFormatStringForFormatting(
      TYPE_DATETIME, "a", "Cannot find matched format element at 0");
}

TEST(DateAndTimeToStringTests, ValidateFormatStringForFormattingTime) {
  // Success case
  for (const auto& element : GetTimeElements()) {
    TestValidateFormatStringForFormatting(TYPE_TIME, element, "");
  }

  // Date elements are not allowed for TIME
  for (const auto& element : GetDateElements()) {
    TestValidateFormatStringForFormatting(
        TYPE_TIME, element,
        absl::Substitute("TIME does not support $0", element));
  }

  // Time zone elements are not allowed for TIME
  for (const auto& element : GetTimeZoneElements()) {
    TestValidateFormatStringForFormatting(
        TYPE_TIME, element,
        absl::Substitute("TIME does not support $0", element));
  }

  // Format string is not valid utf-8 string
  TestValidateFormatStringForFormatting(
      TYPE_TIME, "\xFF\xFF", "Format string is not a valid UTF-8 string");

  // Invalid format element
  TestValidateFormatStringForFormatting(
      TYPE_TIME, "a", "Cannot find matched format element at 0");
}

TEST(DateAndTimeToStringTests, ValidateFormatStringForFormattingTimestamp) {
  // Success case
  for (const auto& element : GetTimeElements()) {
    TestValidateFormatStringForFormatting(TYPE_TIMESTAMP, element, "");
  }
  for (const auto& element : GetDateElements()) {
    TestValidateFormatStringForFormatting(TYPE_TIMESTAMP, element, "");
  }
  for (const auto& element : GetTimeZoneElements()) {
    TestValidateFormatStringForFormatting(TYPE_TIMESTAMP, element, "");
  }

  // Format string is not valid utf-8 string
  TestValidateFormatStringForFormatting(
      TYPE_TIMESTAMP, "\xFF\xFF", "Format string is not a valid UTF-8 string");

  // Invalid format element
  TestValidateFormatStringForFormatting(
      TYPE_TIMESTAMP, "a", "Cannot find matched format element at 0");
}

TEST(DateAndTimeToStringTests, ValidateFormatStringForFormatting) {
  TestValidateFormatStringForFormatting(
      TYPE_INT64, "SS", "Unsupported output type for validation",
      absl::StatusCode::kInvalidArgument);
}

TEST(DateTimeUtilTest, FormatElementParsing) {
  ExecuteFormatElementParsingTest(
      "YYYYYYY",
      {FormatElement(FormatElementType::kYear, "YYYY"),
       FormatElement(FormatElementType::kYear, "YYY")},
      "");
  ExecuteFormatElementParsingTest(
      "DDDDAYDYDD",
      {FormatElement(FormatElementType::kDay, "DDD"),
       FormatElement(FormatElementType::kDay, "DAY"),
       FormatElement(FormatElementType::kDay, "DY"),
       FormatElement(FormatElementType::kDay, "DD")},
      "");
  ExecuteFormatElementParsingTest(
      "YYYY-YYY",
      {FormatElement(FormatElementType::kYear, "YYYY"),
       FormatElement(FormatElementType::kLiteral, "-"),
       FormatElement(FormatElementType::kYear, "YYY")},
      "");
  ExecuteFormatElementParsingTest(
      "YYYY    YYY",
      {FormatElement(FormatElementType::kYear, "YYYY"),
       FormatElement(FormatElementType::kWhitespace, "    "),
       FormatElement(FormatElementType::kYear, "YYY")},
      "");
  // Sequence of whitespace other than ' ' (ASCII 32) will not be parsed as
  // elemenet of "kWhitespace" type. "\u1680" refers to a Unicode whitespace
  // "U+1680".
  ExecuteFormatElementParsingTest(
      "YYYY\n\r\t\u1680YYY",
      {}, "Cannot find matched format element at 4");
  ExecuteFormatElementParsingTest(
      R"(-MMD"abc")",
      {FormatElement(FormatElementType::kLiteral, "-"),
       FormatElement(FormatElementType::kMonth, "MM"),
       FormatElement(FormatElementType::kDay, "D"),
       FormatElement(FormatElementType::kDoubleQuotedLiteral, R"(abc)")},
      "");
  ExecuteFormatElementParsingTest(
      "YYyy-yYy",
      {FormatElement(FormatElementType::kYear, "YYyy"),
       FormatElement(FormatElementType::kLiteral, "-"),
       FormatElement(FormatElementType::kYear, "yYy")},
      "");
  ExecuteFormatElementParsingTest(
      R"("abc\\")",
      {FormatElement(FormatElementType::kDoubleQuotedLiteral, R"(abc\)")}, "");
  ExecuteFormatElementParsingTest(
      R"("def\"")",
      {FormatElement(FormatElementType::kDoubleQuotedLiteral, R"(def")")}, "");
  ExecuteFormatElementParsingTest("random_str", {},
                                  "Cannot find matched format element at 0");

  ExecuteFormatElementParsingTest(
      R"(")", {}, "Cannot find matching \" for quoted literal at 0");
  ExecuteFormatElementParsingTest(
      R"("abc\)", {}, "Cannot find matching \" for quoted literal at 0");

  // Actually what user passed in is '"abc\"', since the ending '"' is escaped
  // by the preceding '\', so it cannot be considered to be the ending '"' of
  // the text element
  ExecuteFormatElementParsingTest(
      R"("abc\")", {}, "Cannot find matching \" for quoted literal at 0");
  ExecuteFormatElementParsingTest(
      R"("abc\t")", {}, "Unsupported escape sequence \\t in text at 0");
  const std::vector<FormatElement>& all_format_elements =
      GetAllSupportedFormatElements();
  std::string all_format_elements_format_string = "";
  for (auto& format_element : all_format_elements) {
    if (format_element.type != FormatElementType::kDoubleQuotedLiteral) {
      absl::StrAppend(&all_format_elements_format_string,
                      format_element.original_str);
    } else {
      absl::StrAppend(&all_format_elements_format_string,
                      "\"" + format_element.original_str + "\"");
    }
  }
  ExecuteFormatElementParsingTest(all_format_elements_format_string,
                                  all_format_elements, "");
}

TEST(DateTimeUtilTest, BasicCastFormatTimestampTest) {
  int64_t timestamp =
      123456789012345;  // Thursday, November 29, 1973 9:33:09 PM
  std::string output;
  ZETASQL_EXPECT_OK(CastFormatTimestampToString("DAY, MONTH DD, YYYY HH:MI:SS AM",
                                        timestamp, absl::UTCTimeZone(),
                                        &output));
  EXPECT_EQ(output, "THURSDAY, NOVEMBER 29, 1973 09:33:09 PM");

  struct CastFormatTimestampTest {
    std::string format_string;
    std::string expected_string;
  } kCastFormatTimestampToStringTests[] = {
      {"YYYY", "1973"},      {"YYY", "973"},
      {"YY", "73"},          {"Y", "3"},
      {"RRRR", "1973"},      {"RR", "73"},
      {"MM", "11"},          {"MON", "NOV"},
      {"MONTH", "NOVEMBER"}, {"Mon", "Nov"},
      {"Month", "November"}, {"mon", "nov"},
      {"month", "november"}, {"DD", "29"},
      {"DDD", "333"},        {"D", "5"},
      {"DAY", "THURSDAY"},   {"DY", "THU"},
      {"HH", "09"},          {"Day", "Thursday"},
      {"Dy", "Thu"},         {"day", "thursday"},
      {"dy", "thu"},         {"HH12", "09"},
      {"HH24", "21"},        {"MI", "33"},
      {"SS", "09"},          {"SSSSS", "77589"},
      {"FF1", "0"},          {"FF2", "01"},
      {"FF3", "012"},        {"FF4", "0123"},
      {"FF5", "01234"},      {"FF6", "012345"},
      {"FF7", "0123450"},    {"FF8", "01234500"},
      {"FF9", "012345000"},  {"A.M.", "P.M."},
      {"P.M.", "P.M."},      {"AM", "PM"},
      {"PM", "PM"},          {"TZH", "+00"},
      {"TZM", "00"}};

  for (const auto& test : kCastFormatTimestampToStringTests) {
    ZETASQL_EXPECT_OK(CastFormatTimestampToString(test.format_string, timestamp,
                                          absl::UTCTimeZone(), &output));
    EXPECT_EQ(output, test.expected_string);
  }
}

static void ExecuteCastDateTimeFunctionTest(const FunctionTestCall& test) {
  if (zetasql_base::StringCaseEqual(test.function_name, "cast_format_timestamp")) {
    TestCastFormatTimestamp(test);
  } else if (zetasql_base::StringCaseEqual(test.function_name, "cast_format_date")) {
    TestCastFormatDate(test);
  } else if (zetasql_base::StringCaseEqual(test.function_name, "cast_format_datetime")) {
    TestCastFormatDatetime(test);
  } else if (zetasql_base::StringCaseEqual(test.function_name, "cast_format_time")) {
    TestCastFormatTime(test);
  }
}

typedef testing::TestWithParam<FunctionTestCall> CastFormatTemplateTest;

TEST_P(CastFormatTemplateTest, CastDateTimeFunctionTests) {
  const FunctionTestCall& test = GetParam();
  ExecuteCastDateTimeFunctionTest(test);
}

INSTANTIATE_TEST_SUITE_P(
    CastTimestampTest, CastFormatTemplateTest,
    testing::ValuesIn(GetFunctionTestsCastFormatDateTimestamp()));

TEST(DateTimeUtilTest, UnsupportedCastFormatTimestampTest) {
  int64_t timestamp =
      123456789012345;  // Thursday, November 29, 1973 9:33:09 PM

  // Strings that are parseable, but currently aren't in the road map to
  // support. See any format element that has C3 label:
  // (broken link)
  std::string unsupportedFormatTest[] = {"IYY", "IY", "I",  "RM", "J", "CC",
                                         "SCC", "Q",  "IW", "WW", "W", "FM"};
  for (const auto& test : unsupportedFormatTest) {
    std::string output;
    auto status = CastFormatTimestampToString(test, timestamp,
                                              absl::UTCTimeZone(), &output);
    EXPECT_THAT(status, StatusIs(absl::StatusCode::kOutOfRange,
                                 HasSubstr("Unsupported format")));
  }
}

TEST(DateTimeUtilTest, NonTraditionalYearTest) {
  const int64_t three_digit_year = absl::ToUnixMicros(absl::FromCivil(
      absl::CivilSecond(776, 1, 31, 12, 10, 05), absl::UTCTimeZone()));

  const int64_t two_digit_year = absl::ToUnixMicros(absl::FromCivil(
      absl::CivilSecond(76, 1, 31, 12, 10, 05), absl::UTCTimeZone()));

  const int64_t one_digit_year = absl::ToUnixMicros(absl::FromCivil(
      absl::CivilSecond(6, 1, 31, 12, 10, 05), absl::UTCTimeZone()));

  struct CastFormatTimestampTest {
    int64_t timestamp;
    std::string format_string;
    std::string expected_string;
  } kCastFormatTimestampToStringTests[] = {
      {three_digit_year, "YYYY", "0776"}, {three_digit_year, "YYY", "776"},
      {two_digit_year, "YYYY", "0076"},   {two_digit_year, "YYY", "076"},
      {one_digit_year, "YYYY", "0006"},   {one_digit_year, "YYY", "006"}};
  for (const auto& test : kCastFormatTimestampToStringTests) {
    std::string output;
    ZETASQL_EXPECT_OK(CastFormatTimestampToString(test.format_string, test.timestamp,
                                          absl::UTCTimeZone(), &output));
    EXPECT_EQ(output, test.expected_string);
  }
}

}  // namespace
}  // namespace functions
}  // namespace zetasql