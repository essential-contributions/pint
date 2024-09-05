use crate::{error::Error, warning::Warning};
use core::cell::RefCell;

/// A handler with which you can emit diagnostics.
#[derive(Default, Debug)]
pub struct Handler {
    /// The inner handler.
    /// This construction is used to avoid `&mut` all over the compiler.
    inner: RefCell<HandlerInner>,
}

/// Contains the actual data for `Handler`.
/// Modelled this way to afford an API using interior mutability.
#[derive(Default, Debug)]
pub struct HandlerInner {
    /// The sink through which errors and warnings will be emitted.
    pub errors: Vec<Error>,
    pub warnings: Vec<Warning>,
}

impl Handler {
    /// Emit the error `err`.
    pub fn emit_err(&self, err: Error) -> ErrorEmitted {
        self.inner.borrow_mut().errors.push(err);
        ErrorEmitted { _priv: () }
    }

    /// Emit the warning `warn`.
    pub fn emit_warn(&self, warning: Warning) -> WarningEmitted {
        self.inner.borrow_mut().warnings.push(warning);
        WarningEmitted { _priv: () }
    }

    /// Compilation should be cancelled.
    pub fn cancel(&self) -> ErrorEmitted {
        ErrorEmitted { _priv: () }
    }
    // TODO determine if we need cancel to involve warnings -- doubt it?

    /// Prove warnings were emitted without cancelling compilation
    pub fn continue_with_warnings(&self) -> WarningEmitted {
        WarningEmitted { _priv: () }
    }

    /// Produce a `Result::Ok(value)` if the handler has no errors. Otherwise, produce
    /// `Result::Err(_)`
    pub fn result<T>(&self, value: T) -> Result<T, ErrorEmitted> {
        if self.has_errors() {
            Err(self.cancel())
        } else {
            Ok(value)
        }
    }

    pub fn has_errors(&self) -> bool {
        !self.inner.borrow().errors.is_empty()
    }

    pub fn has_warnings(&self) -> bool {
        !self.inner.borrow().warnings.is_empty()
    }

    pub fn clear(&self) {
        self.clear_errors();
        self.clear_warnings();
    }

    pub fn clear_errors(&self) {
        self.inner.borrow_mut().errors.clear();
    }

    pub fn clear_warnings(&self) {
        self.inner.borrow_mut().warnings.clear();
    }

    pub fn scope<T>(
        &self,
        f: impl FnOnce(&Handler) -> Result<T, ErrorEmitted>,
    ) -> Result<T, ErrorEmitted> {
        let scoped_handler = Handler::default();
        let closure_res = f(&scoped_handler);
        let had_errors = scoped_handler.has_errors();

        self.append(scoped_handler);

        if had_errors {
            Err(ErrorEmitted { _priv: () })
        } else {
            closure_res
        }
    }

    pub fn consume(self) -> HandlerInner {
        self.inner.into_inner()
    }

    pub fn append(&self, other: Handler) {
        let other_inner = other.consume();
        for err in other_inner.errors {
            self.emit_err(err);
        }
        for warn in other_inner.warnings {
            self.emit_warn(warn);
        }
    }
}

/// Proof that an error was emitted through a `Handler`.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ErrorEmitted {
    _priv: (),
}

/// Proof that a warning was emitted through a `Handler`.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct WarningEmitted {
    _priv: (),
}
