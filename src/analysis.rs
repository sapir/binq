pub mod code_flow;
pub mod data_flow;
pub mod data_sizes;

use crate::database::Database;

pub fn analyze(db: &mut Database) {
    code_flow::add_code_flow(db);
    data_flow::analyze_data_flow(db);
    data_sizes::analyze_data_sizes(db);
}
