pub mod code_flow;

use crate::{database::Database, ir::Statement};

fn index_statements_by_imark(db: &mut Database) {
    db.addr_to_entity = db
        .world
        .query_mut::<&Statement>()
        .into_iter()
        .filter_map(|(entity, stmt)| match stmt {
            Statement::IMark { addr, .. } => Some((*addr, entity)),
            _ => None,
        })
        .collect();
}

pub fn analyze(db: &mut Database) {
    index_statements_by_imark(db);
    code_flow::add_code_flow(db);
}
