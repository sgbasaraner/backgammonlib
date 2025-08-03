use crate::game::MovePosition;
use micromap::Map;

pub type PositionPieceMapInner = Map<MovePosition, u8, 26>;

#[derive(Debug, Clone)]
pub struct PositionPieceMap {
    inner: PositionPieceMapInner,
}

impl Default for PositionPieceMap {
    fn default() -> Self {
        let mut pos_map = PositionPieceMapInner::new();
        pos_map.insert(MovePosition::TwentyFour, 2);
        pos_map.insert(MovePosition::Thirteen, 5);
        pos_map.insert(MovePosition::Eight, 3);
        pos_map.insert(MovePosition::Six, 5);
        Self { inner: pos_map }
    }
}

impl PositionPieceMap {
    pub fn from_inner(inner: PositionPieceMapInner) -> Self {
        Self { inner }
    }

    pub fn empty() -> Self {
        Self { inner: Map::new() }
    }

    pub fn non_empty_position_count(&self) -> u8 {
        self.iter().count() as u8
    }
    pub fn debug_serialize(&self) -> Vec<String> {
        self.inner
            .iter()
            .map(|(k, v)| format!("{:?}, {:?}", k.board_number(), v))
            .collect()
    }

    pub fn piece_count_at(&self, pos: &MovePosition) -> u8 {
        self.inner.get(pos).cloned().unwrap_or_default()
    }

    fn increment_position(&mut self, pos: &MovePosition) {
        let old_player_to_val = self.inner.get(pos).unwrap_or(&0);
        let new_player_to_val = old_player_to_val + 1;
        self.inner.insert(*pos, new_player_to_val);
    }

    fn force_decrement_pos(&mut self, pos: &MovePosition) {
        let old_player_pos_val = self.inner.get(pos).unwrap();
        let new_player_pos_val = old_player_pos_val - 1;
        if new_player_pos_val == 0 {
            self.inner.remove(pos);
        } else {
            self.inner.insert(*pos, new_player_pos_val);
        }
    }

    pub fn force_move_piece(&mut self, from: &MovePosition, to: &MovePosition) {
        self.force_decrement_pos(from);
        self.increment_position(to);
    }

    pub fn iter(&self) -> impl Iterator<Item = (&MovePosition, &u8)> {
        self.inner.iter()
    }
}

impl PartialEq for PositionPieceMap {
    fn eq(&self, other: &Self) -> bool {
        if self.non_empty_position_count() != other.non_empty_position_count() {
            return false;
        }

        for (mp, count) in self.iter() {
            if *count != other.piece_count_at(mp) {
                return false;
            }
        }

        true
    }
}
