pub mod code_flow;

use crate::database::Database;

pub fn analyze(db: &mut Database) {
    code_flow::add_code_flow(db);
}
