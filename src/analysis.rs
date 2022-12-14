pub mod code_flow;

use crate::database::Database;

pub fn analyze(db: &mut Database) {
    db.reindex();
    code_flow::add_code_flow(db);
}
