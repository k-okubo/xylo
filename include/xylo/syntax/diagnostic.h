
#ifndef XYLO_SYNTAX_DIAGNOSTIC_H_
#define XYLO_SYNTAX_DIAGNOSTIC_H_

#include <stdexcept>
#include <string>
#include <utility>

#include "xylo/syntax/location.h"
#include "xylo/util/vector.h"

namespace xylo {


enum class DiagnosticSeverity {
  kError,
  kWarning,
  kNote,
};


struct Diagnostic {
  DiagnosticSeverity severity;
  SourceRange position;
  std::string message;

  friend std::ostream& operator<<(std::ostream& stream, const Diagnostic& diagnostic) {
    auto& position = diagnostic.position;

    if (position.start.line > 0) {
      if (position.file != nullptr) {
        stream << position.file->path() << ":";
      } else {
        stream << "<input>:";
      }
      stream << position.start.line << ":" << position.start.column << ": ";
    }

    switch (diagnostic.severity) {
      case DiagnosticSeverity::kError:
        stream << "error: ";
        break;
      case DiagnosticSeverity::kWarning:
        stream << "warning: ";
        break;
      case DiagnosticSeverity::kNote:
        stream << "note: ";
        break;
    }

    stream << diagnostic.message;
    return stream;
  }
};


class DiagnosticReporter {
 public:
  DiagnosticReporter() = default;
  virtual ~DiagnosticReporter() = default;

  bool has_diagnostics() const { return !diagnostics_.empty(); }
  const Vector<Diagnostic>& diagnostics() const { return diagnostics_; }

  bool has_errors() const {
    for (const auto& d : diagnostics_) {
      if (d.severity == DiagnosticSeverity::kError) {
        return true;
      }
    }
    return false;
  }

  int error_count() const {
    int count = 0;
    for (const auto& d : diagnostics_) {
      if (d.severity == DiagnosticSeverity::kError) {
        count += 1;
      }
    }
    return count;
  }

 protected:
  void Report(DiagnosticSeverity severity, const SourceRange& position, std::string&& message) {
    Diagnostic d;
    d.position = position;
    d.message = std::move(message);
    d.severity = severity;
    diagnostics_.push_back(std::move(d));

    if (error_count() > 10) {
      throw std::runtime_error("too many errors, aborting");
    }
  }

  void ReportError(const SourceRange& position, std::string&& message) {
    Report(DiagnosticSeverity::kError, position, std::move(message));
  }

  void ReportWarning(const SourceRange& position, std::string&& message) {
    Report(DiagnosticSeverity::kWarning, position, std::move(message));
  }

  void ReportNote(const SourceRange& position, std::string&& message) {
    Report(DiagnosticSeverity::kNote, position, std::move(message));
  }

 private:
  Vector<Diagnostic> diagnostics_;
};


}  // namespace xylo

#endif  // XYLO_SYNTAX_DIAGNOSTIC_H_
