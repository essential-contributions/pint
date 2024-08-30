use crate::error::Error;
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
struct HandlerInner {
    /// The sink through which errors and warnings will be emitted.
    errors: Vec<Error>,
    warnings: Vec<Error>,
}

impl Handler {
    /// Emit the error `err`.
    pub fn emit_err(&self, err: Error) -> ErrorEmitted {
        self.inner.borrow_mut().errors.push(err);
        ErrorEmitted { _priv: () }
    }

    /// Emit the warning `warn`.
    pub fn emit_warn(&self, warn: Error) -> WarningEmitted {
        self.inner.borrow_mut().warnings.push(warn);
        WarningEmitted { _priv: () }
    }

    /// Compilation should be cancelled.
    pub fn cancel(&self) -> ErrorEmitted {
        ErrorEmitted { _priv: () }
    }
    // TODO determine if we need cancel to involve warnings

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

    // TODO: Figure out below. Do we want to receive all warnings and errors? or just get each individually?

    /// Extract all the warnings and errors from this handler.
    pub fn consume(self) -> Vec<Error> {
        let inner = self.inner.into_inner();
        inner.errors
    }

    pub fn append(&self, other: Handler) {
        let errors = other.consume();
        for err in errors {
            self.emit_err(err);
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
