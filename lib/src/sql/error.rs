use fmt::Write;
use nom::error::ContextError;
use nom::error::ErrorKind;
use nom::error::ParseError;
use nom::Err;
use nom::Offset;
use std::fmt;
use thiserror::Error;

pub const VALUE: &str = "a SurrealQL value";
pub const PARAM: &str = "a valid parameter name";
pub const STRING: &str = "a valid string";
pub const ARRAY: &str = "a list of values";
pub const OBJECT: &str = "a key value object pair";
pub const STATEMENT: &str = "a BEGIN, CANCEL, COMMIT, CREATE, DEFINE, DELETE, IF, INFO, INSERT, KILL, LET, LIVE, OPTION, RELATE, REMOVE, RETURN, SELECT, SLEEP, UPDATE, or USE statement";

#[derive(Error, Debug)]
pub enum Error<I> {
	Parser(VerboseError<I>),
	Field(I, String),
	Split(I, String),
	Order(I, String),
	Group(I, String),
}

pub type IResult<I, O, E = Error<I>> = Result<(I, O), Err<E>>;

impl<I> ParseError<I> for Error<I> {
	fn from_error_kind(i: I, k: ErrorKind) -> Self {
		Self::Parser(VerboseError {
			errors: vec![(i, VerboseErrorKind::Nom(k))],
		})
	}
	fn from_char(i: I, c: char) -> Self {
		Self::Parser(VerboseError {
			errors: vec![(i, VerboseErrorKind::Char(c))],
		})
	}
	fn append(_: I, _: ErrorKind, other: Self) -> Self {
		other
	}
}

impl<I> ContextError<I> for Error<I>
where
	I: std::fmt::Debug + std::fmt::Display,
{
	fn add_context(i: I, c: &'static str, other: Self) -> Self {
		match other {
			Self::Parser(mut other) => {
				other.errors.push((i, VerboseErrorKind::Context(c)));
				Self::Parser(other)
			}
			_ => other,
		}
	}
}

#[derive(Clone, Debug, PartialEq)]
pub struct VerboseError<I> {
	pub errors: Vec<(I, VerboseErrorKind)>,
}

/// Error context for `VerboseError`
#[derive(Clone, Debug, PartialEq)]
pub enum VerboseErrorKind {
	/// Static string added by the `context` function
	Context(&'static str),
	/// Indicates which character was expected by the `char` function
	Char(char),
	/// Error kind given by various nom parsers
	Nom(ErrorKind),
}

impl<I: fmt::Display> fmt::Display for VerboseError<I> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		writeln!(f, "Parse error:")?;
		for (input, error) in &self.errors {
			match error {
				VerboseErrorKind::Nom(e) => writeln!(f, "{:?} at: {}", e, input)?,
				VerboseErrorKind::Char(c) => writeln!(f, "expected '{}' at: {}", c, input)?,
				VerboseErrorKind::Context(s) => writeln!(f, "in section '{}', at: {}", s, input)?,
			}
		}

		Ok(())
	}
}

pub fn convert_error<I: core::ops::Deref<Target = str>>(input: I, e: VerboseError<I>) -> String {
	// Create a new emty string
	let mut result = String::from("\n\n");
	// Loop over each accumulated error
	for (i, (substring, kind)) in e.errors.iter().enumerate() {
		// Get the initial offset of the error
		let offset = input.offset(substring);
		// Check if the input was empty
		if input.is_empty() {
			match kind {
				VerboseErrorKind::Char(c) => {
					write!(&mut result, "{}: expected '{}', got empty input\n\n", i, c)
				}
				VerboseErrorKind::Context(s) => {
					write!(&mut result, "{}: expected {}, got empty input\n\n", i, s)
				}
				VerboseErrorKind::Nom(e) => {
					write!(&mut result, "{}: in {:?}, got empty input\n\n", i, e)
				}
			}
		} else {
			// Get the input prefix for this error
			let prefix = &input.as_bytes()[..offset];
			// Get the line on which this error occurs
			let line_number = prefix.iter().filter(|&&b| b == b'\n').count() + 1;
			// Find the point at which the line error begins
			let line_begin =
				prefix.iter().rev().position(|&b| b == b'\n').map(|pos| offset - pos).unwrap_or(0);
			// Get the full line after the previous new line
			let line =
				input[line_begin..].lines().next().unwrap_or(&input[line_begin..]).trim_end();
			// The (1-indexed) column number is the offset of our substring into that line
			let column = line.offset(substring) + 1;
			// Display the error depending on the type
			match kind {
				VerboseErrorKind::Char(c) => {
					if let Some(v) = substring.chars().next() {
						write!(
							&mut result,
							"{i}: at line {line_number}:\n\
							{line}\n\
							{caret:>column$}\n\
							expected '{c}', found {v}\n",
							caret = '^',
						)
					} else {
						write!(
							&mut result,
							"{i}: at line {line_number}:\n\
							{line}\n\
							{caret:>column$}\n\
							expected '{c}', got end of input\n",
							caret = '^',
						)
					}
				}
				VerboseErrorKind::Context(c) => write!(
					&mut result,
					"{i}: at line {line_number}, expected {c}:\n\
					{line}\n\
					{caret:>column$}\n",
					caret = '^',
				),
				VerboseErrorKind::Nom(e) => write!(
					&mut result,
					"{i}: at line {line_number}, in {e:?}:\n\
					{line}\n\
					{caret:>column$}\n",
					caret = '^',
				),
			}
		}
		.unwrap();
	}
	// Return the formatted error
	result
}
