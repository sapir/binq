pub mod code_flow;
pub mod data_flow;

use crate::database::Database;

pub fn analyze(db: &mut Database) {
    code_flow::add_code_flow(db);
    data_flow::analyze_data_flow(db);
}
