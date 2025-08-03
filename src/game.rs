use crate::parser::ParsingErrorType;
use crate::piece_map::PositionPieceMap;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum DiceValue {
    One,
    Two,
    Three,
    Four,
    Five,
    Six,
}

impl DiceValue {
    pub fn arithmetic_value(&self) -> u8 {
        match self {
            DiceValue::One => 1,
            DiceValue::Two => 2,
            DiceValue::Three => 3,
            DiceValue::Four => 4,
            DiceValue::Five => 5,
            DiceValue::Six => 6,
        }
    }
}

impl TryFrom<u8> for DiceValue {
    type Error = ParsingErrorType;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(DiceValue::One),
            2 => Ok(DiceValue::Two),
            3 => Ok(DiceValue::Three),
            4 => Ok(DiceValue::Four),
            5 => Ok(DiceValue::Five),
            6 => Ok(DiceValue::Six),
            _ => Err(ParsingErrorType::InvalidDice),
        }
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub struct DiceRoll {
    pub dice_1: DiceValue,
    pub dice_2: DiceValue,
}

impl DiceRoll {
    pub fn is_double(&self) -> bool {
        return self.dice_1.arithmetic_value() == self.dice_2.arithmetic_value();
    }
}

#[derive(Debug)]
pub struct MoveTuple {
    pub move_no: usize,
    pub move_1: Option<Move>,
    pub move_2: Option<Move>,
}

impl MoveTuple {
    pub fn moves(&self) -> Vec<&Move> {
        let mut moves = vec![];

        if let Some(move_1) = &self.move_1 {
            moves.push(move_1);
        }

        if let Some(move_2) = &self.move_2 {
            moves.push(move_2);
        }

        moves
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Player {
    P1,
    P2,
}

impl Player {
    pub fn other(&self) -> Player {
        return match self {
            Player::P1 => Player::P2,
            Player::P2 => Player::P1,
        };
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum MoveError {
    ResponseWithoutDoubleOffer,
    HaveToRespond,
    HaveToMoveOffFromBar,
    PlayPossible,
    InvalidBoardNumber(u8),
    InvalidPlayCount,
    TargetOutOfBounds,
    TargetIsBar,
    SourceIsOff,
    GameEnded,
    NoPieceInSource,
    TargetOccupied,
    IllegalBearOff,
    IllegalDouble,
    PlayDiceMismatch(DiceRoll),
}

#[derive(Debug)]
pub struct GameState {
    pub past_moves: Vec<MoveTuple>,
    pub multiplier: usize,
    pub p1_positions: PositionPieceMap,
    pub p2_positions: PositionPieceMap,
    pub winner: Option<(Player, usize)>,
}

impl Default for GameState {
    fn default() -> Self {
        Self {
            past_moves: vec![],
            multiplier: 1,
            p1_positions: Default::default(),
            p2_positions: Default::default(),
            winner: None,
        }
    }
}

impl GameState {
    pub fn new() -> GameState {
        Self::default()
    }

    pub fn add_move(&mut self, candidate: Move) -> Result<(), MoveError> {
        if self.winner.is_some() {
            return Err(MoveError::GameEnded);
        }
        self.validate_answer_issues(&candidate)?;
        let side = self.get_side_to_play();
        match &candidate {
            Move::Skip => {
                if side == Player::P2 || !self.past_moves.is_empty() {
                    return Err(MoveError::HaveToRespond);
                }
            }
            Move::CantPlay(dice_roll) => {
                let cur_player_pos = self.get_player_positions(&side);
                let cur_opp_pos = self.get_player_positions(&side.other());
                Self::validate_no_play(&dice_roll, cur_player_pos, cur_opp_pos)?;
            }
            Move::Plays((roll, plays)) => {
                self.validate_play(&roll, &plays)?;
            }
            _ => (),
        };

        self.apply_move(candidate, &side);

        Ok(())
    }

    fn get_side_to_play(&self) -> Player {
        let side: Player = self
            .past_moves
            .last()
            .map(|m| {
                if m.move_2.is_some() {
                    Player::P1
                } else {
                    Player::P2
                }
            })
            .unwrap_or(Player::P1);
        side
    }

    fn apply_move(&mut self, mov: Move, side: &Player) {
        let mut winner: Option<(Player, usize)> = None;
        match &mov {
            Move::Plays((_, plays)) => {
                let mut pp_mut = self.get_player_positions(side).clone();
                let mut opp_mut = self.get_player_positions(&side.other()).clone();
                for play in plays {
                    assert!(Self::make_play_mutating(
                        &play.from,
                        &play.to,
                        &mut pp_mut,
                        &mut opp_mut
                    )
                    .is_ok());
                }

                if pp_mut.piece_count_at(&MovePosition::Off) > 0 && pp_mut.non_empty_position_count() == 1 {
                    // game won
                    let opponent_borne_any = opp_mut.piece_count_at(&MovePosition::Off) > 0;
                    let game_result_multiplier = if opponent_borne_any { 1 } else { 2 };
                    let score = self.multiplier * game_result_multiplier;
                    winner = Some((side.clone(), score))
                }

                match side {
                    Player::P1 => {
                        self.p1_positions = pp_mut;
                        self.p2_positions = opp_mut;
                    }
                    Player::P2 => {
                        self.p1_positions = opp_mut;
                        self.p2_positions = pp_mut;
                    }
                }

            }
            Move::Take => self.multiplier *= 2,
            Move::Drop => winner = Some((side.other(), self.multiplier)),
            _ => {}
        }

        let create_new_move = self
            .past_moves
            .last()
            .map(|last_entry| last_entry.move_2.is_some())
            .unwrap_or(true);
        if create_new_move {
            self.past_moves.push(MoveTuple {
                move_no: self
                    .past_moves
                    .last()
                    .map(|m| m.move_no)
                    .unwrap_or_default()
                    + 1,
                move_1: Some(mov),
                move_2: None,
            });
        } else {
            self.past_moves.last_mut().unwrap().move_2 = Some(mov);
        }

        self.winner = winner;
    }

    pub fn validate_play(&self, dice_roll: &DiceRoll, plays: &Vec<Play>) -> Result<(), MoveError> {
        if plays.is_empty() {
            return Err(MoveError::InvalidPlayCount);
        }
        let side: Player = self
            .get_side_to_play();
        let player_positions = self.get_player_positions(&side);
        let opp_positions = self.get_player_positions(&side.other());

        let matches_any_known_play =
            Self::all_possible_moves(dice_roll, player_positions, opp_positions).any(
                |sequence_iterator| {
                    let seq_collected: Vec<Play> = sequence_iterator.collect();

                    seq_collected.iter().eq(plays)
                },
            );

        // let all_possible_moves_collected: Vec<Vec<Play>> =
        //     GameState::all_possible_moves(&dice_roll, player_positions, opp_positions)
        //         .into_iter()
        //         .map(|mv| mv.collect())
        //         .collect();
        //
        // let pp_1: Vec<String> = player_positions.debug_serialize();
        // let pp_opp: Vec<String> = opp_positions.debug_serialize();

        if !matches_any_known_play {
            Err(MoveError::PlayDiceMismatch(dice_roll.clone()))
        } else {
            Ok(())
        }
    }

    fn make_play_mutating(
        from: &MovePosition,
        to: &MovePosition,
        player_pos_map: &mut PositionPieceMap,
        opp_pos_map: &mut PositionPieceMap,
    ) -> Result<(), MoveError> {
        // move piece
        player_pos_map.force_move_piece(from, to);

        // send enemy piece to bar, if needed
        if let Some(to_converted_to_opp_coordinates) = to.translate_to_opponent_pov() {
            let opponent_piece_count = opp_pos_map.piece_count_at(&to_converted_to_opp_coordinates);
            match opponent_piece_count {
                0 => (),
                1 => {
                    opp_pos_map
                        .force_move_piece(&to_converted_to_opp_coordinates, &MovePosition::Bar);
                }
                _ => {
                    panic!("Unexpected piece count at enemy piece location, expected 0 or 1");
                }
            }
        }

        Ok(())
    }

    fn validate_answer_issues(&self, candidate: &Move) -> Result<(), MoveError> {
        let last_move = self.last_move();

        let have_to_answer = match last_move {
            Some(m) => match m {
                Move::Double(next_multiplier) => {
                    self.validate_double(*next_multiplier)?;
                    true
                }
                _ => false,
            },
            None => false,
        };
        if have_to_answer {
            match *candidate {
                Move::Take => Ok(()),
                Move::Drop => Ok(()),
                _ => Err(MoveError::HaveToRespond),
            }
        } else {
            match *candidate {
                Move::Take => Err(MoveError::ResponseWithoutDoubleOffer),
                Move::Drop => Err(MoveError::ResponseWithoutDoubleOffer),
                _ => Ok(()),
            }
        }
    }

    fn validate_no_play(
        roll: &DiceRoll,
        player_pos_map: &PositionPieceMap,
        opp_pos_map: &PositionPieceMap,
    ) -> Result<(), MoveError> {
        let any_play_possible: bool = Self::any_play_possible(&roll, &player_pos_map, &opp_pos_map);

        if any_play_possible {
            Err(MoveError::PlayPossible)
        } else {
            Ok(())
        }
    }

    fn validate_double(
        &self,
        multiplier: usize,
    ) -> Result<(), MoveError> {
        let side_to_play = self.get_side_to_play();
        let mut last_doubler: Option<Player> = None;

        for mov in self.past_moves.iter().rev() {
            match mov.move_2.as_ref() {
                None => {}
                Some(m) => match m {
                    Move::Double(_) => {
                        last_doubler = Some(Player::P2);
                        break;
                    }
                    _ => {}
                }
            }
            match mov.move_1.as_ref() {
                None => {}
                Some(m) => match m {
                    Move::Double(_) => {
                        last_doubler = Some(Player::P1);
                        break;
                    }
                    _ => {}
                }
            }
        }


        if last_doubler == Some(side_to_play) {
            Err(MoveError::IllegalDouble)
        } else {
            if multiplier == self.multiplier * 2 {
                Ok(())
            } else {
                Err(MoveError::IllegalDouble)
            }
        }

    }

    pub fn any_play_possible(
        roll: &DiceRoll,
        player_pos_map: &PositionPieceMap,
        opp_pos_map: &PositionPieceMap,
    ) -> bool {
        Self::all_possible_moves(roll, player_pos_map, opp_pos_map)
            .next()
            .and_then(|mut ms| ms.next())
            .is_some()
    }



    pub fn all_possible_actions(&self, dice_roll: &DiceRoll) -> Vec<Move> {
        if let Some(lm) = self.last_move() {
            match lm {
                Move::Double(_) => {
                    return vec![Move::Take, Move::Drop];
                }
                _ => {}
            }
        }

        let mut all_possible_actions = if self.validate_double(self.multiplier * 2).is_ok() {
            vec![
                Move::Double(self.multiplier * 2)
            ]
        } else {
            Vec::new()
        };

        let side = self.get_side_to_play();
        let mut possible_moves_iter = Self::all_possible_moves(dice_roll, self.get_player_positions(&side), self.get_player_positions(&side.other()));
        let mut no_moves_possible = true;
        for mov in possible_moves_iter {
            no_moves_possible = false;
            all_possible_actions.push(Move::Plays((dice_roll.clone(), mov.collect())))
        }

        if no_moves_possible {
            all_possible_actions.push(Move::CantPlay(dice_roll.clone()));
        }

        all_possible_actions
    }

    fn all_possible_moves<'a>(
        dice_sequences: &'a DiceRoll,
        player_pos_map: &'a PositionPieceMap,
        opp_pos_map: &'a PositionPieceMap,
    ) -> impl Iterator<Item = impl Iterator<Item = Play>> + 'a {
        let dice_sequences = make_dice_sequences(dice_sequences);
        IntoIterator::into_iter(dice_sequences).flat_map(|dice_sequence| {
            Self::all_possible_moves_with_sequence(&dice_sequence, player_pos_map, opp_pos_map)
        })
    }

    fn all_possible_moves_with_sequence(
        dice_sequence: &Vec<DiceValue>,
        player_pos_map: &PositionPieceMap,
        opp_pos_map: &PositionPieceMap,
    ) -> impl Iterator<Item = impl Iterator<Item = Play>> {
        let dice_sequence = dice_sequence.clone();
        let initial_state = (player_pos_map.clone(), opp_pos_map.clone());
        let mut queue = vec![(vec![], dice_sequence, initial_state)];

        std::iter::from_fn(move || {
            while let Some((moves_so_far, remaining_dice, state)) = queue.pop() {
                if remaining_dice.is_empty() {
                    let sequence = moves_so_far.clone();
                    return Some(sequence.into_iter());
                }

                let die = remaining_dice[0];
                let rest_dice = &remaining_dice[1..];
                let mut moved = false;

                let possible_moves = Self::generate_possible_moves_unvalidated(&die, &state.0);
                for (from, to) in possible_moves {
                    if Self::can_move(&from, &to, &state.0, &state.1, &die).is_ok() {
                        let mut new_player_map = state.0.clone();
                        let mut new_opp_map = state.1.clone();

                        let to_opponent_value = to.translate_to_opponent_pov();
                        let is_hit = to_opponent_value
                            .map(|pos| new_opp_map.piece_count_at(&pos) == 1)
                            .unwrap_or_default();

                        if Self::make_play_mutating(
                            &from,
                            &to,
                            &mut new_player_map,
                            &mut new_opp_map,
                        )
                        .is_ok()
                        {
                            let mut new_moves = moves_so_far.clone();
                            new_moves.push(Play {
                                to: to.clone(),
                                from: from.clone(),
                                hit_status: if is_hit {
                                    HitStatus::Hit
                                } else {
                                    HitStatus::NoHit
                                },
                            });
                            let new_state = (new_player_map, new_opp_map);

                            queue.push((new_moves, rest_dice.to_vec(), new_state));
                            moved = true;
                        }
                    }
                }

                // If no moves possible but we made some already
                if !moved && !moves_so_far.is_empty() {
                    return Some(moves_so_far.into_iter());
                }
            }

            None
        })
    }

    fn generate_possible_moves_unvalidated<'a>(
        roll: &'a DiceValue,
        player_pos_map: &'a PositionPieceMap,
    ) -> impl Iterator<Item = (MovePosition, MovePosition)> + 'a {
        player_pos_map
            .iter()
            .filter(|(_, count)| **count > 0)
            .map(|(pp, _)| (pp, pp.plus_dice(roll, true)))
            .filter(|(_, r)| r.is_ok())
            .map(|(pp, r)| (pp.clone(), r.unwrap()))
    }

    fn can_move(
        from: &MovePosition,
        to: &MovePosition,
        player_pos_map: &PositionPieceMap,
        opp_pos_map: &PositionPieceMap,
        dice_value: &DiceValue,
    ) -> Result<(), MoveError> {
        match from {
            MovePosition::Off => Err(MoveError::SourceIsOff),
            _ => {
                // no piece to play
                if player_pos_map.piece_count_at(from) < 1 {
                    return Err(MoveError::NoPieceInSource);
                }

                // there are pieces in bar and another move is being made
                if from != &MovePosition::Bar
                    && player_pos_map.piece_count_at(&MovePosition::Bar) >= 1
                {
                    return Err(MoveError::HaveToMoveOffFromBar);
                }

                match to {
                    MovePosition::Bar => Err(MoveError::TargetIsBar),
                    MovePosition::Off => {
                        if !Self::is_bear_off_eligible(player_pos_map, from, dice_value) {
                            Err(MoveError::IllegalBearOff)
                        } else {
                            Ok(())
                        }
                    }
                    _ => {
                        let to_opp_coord = to.translate_to_opponent_pov().unwrap();
                        if opp_pos_map.piece_count_at(&to_opp_coord) > 1 {
                            Err(MoveError::TargetOccupied)
                        } else {
                            Ok(())
                        }
                    }
                }
            }
        }
    }

    pub fn is_bear_off_eligible(
        pos_map: &PositionPieceMap,
        from: &MovePosition,
        dice_value: &DiceValue,
    ) -> bool {
        let max_bear_off_bn = MovePosition::Six.board_number();

        let bn = from.board_number();
        if bn == 0 || from.board_number() > max_bear_off_bn {
            return false;
        }

        let all_pieces_in_bear_off_zone = pos_map
            .iter()
            .all(|(p, _)| p.board_number() <= max_bear_off_bn);

        if !all_pieces_in_bear_off_zone {
            return false;
        }

        let is_exact_dice_match = bn == dice_value.arithmetic_value();
        if is_exact_dice_match {
            return true;
        }

        dice_value.arithmetic_value() > bn && pos_map.iter().all(|(p, _)| p.board_number() <= bn)
    }

    pub fn get_player_positions(&self, player: &Player) -> &PositionPieceMap {
        match player {
            Player::P1 => &self.p1_positions,
            Player::P2 => &self.p2_positions,
        }
    }

    pub fn last_move(&self) -> Option<&Move> {
        self.past_moves
            .last()
            .map(|m| m.move_2.as_ref().unwrap_or(m.move_1.as_ref().unwrap()))
    }
}

fn make_dice_sequences(dice_roll: &DiceRoll) -> Vec<Vec<DiceValue>> {
    let possible_dice_sequences = if dice_roll.is_double() {
        vec![vec![
            dice_roll.dice_1,
            dice_roll.dice_1,
            dice_roll.dice_1,
            dice_roll.dice_1,
        ]]
    } else {
        vec![
            vec![dice_roll.dice_1, dice_roll.dice_2],
            vec![dice_roll.dice_2, dice_roll.dice_1],
        ]
    };
    possible_dice_sequences
}

#[derive(Debug, Default)]
pub struct Game {
    pub name: Option<String>,
    pub p1_name: Option<String>,
    pub p2_name: Option<String>,
    pub state: GameState,
    pub p1_points: Option<usize>,
    pub p2_points: Option<usize>,
}

impl Game {
    pub fn new() -> Self {
        Self {
            name: None,
            p1_name: None,
            p2_name: None,
            state: GameState::new(),
            p1_points: None,
            p2_points: None,
        }
    }
}

pub struct Match {
    pub name: Option<String>,
    pub games: Vec<Game>,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Move {
    CantPlay(DiceRoll),
    Skip,
    Plays((DiceRoll, Vec<Play>)),
    Double(usize),
    Take,
    Drop,
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum HitStatus {
    Hit,
    NoHit,
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub struct Play {
    pub hit_status: HitStatus,
    pub from: MovePosition,
    pub to: MovePosition,
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum MovePosition {
    Bar,
    Off,
    One,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Ten,
    Eleven,
    Twelve,
    Thirteen,
    Fourteen,
    Fifteen,
    Sixteen,
    Seventeen,
    Eighteen,
    Nineteen,
    Twenty,
    TwentyOne,
    TwentyTwo,
    TwentyThree,
    TwentyFour,
}

impl MovePosition {
    pub fn translate_to_opponent_pov(&self) -> Option<MovePosition> {
        if *self == Self::Bar || *self == Self::Off {
            return None;
        }
        let num = self.board_number();
        let new_num = if num <= 12 {
            let diff_to_12 = 12 - num;
            13 + diff_to_12
        } else {
            let diff_to_13 = num - 13;
            12 - diff_to_13
        };

        Some(MovePosition::from_board_number(new_num).unwrap())
    }

    pub fn plus_dice(
        &self,
        dice: &DiceValue,
        allow_bear_off: bool,
    ) -> Result<MovePosition, MoveError> {
        let dice_val = dice.arithmetic_value();
        match self {
            MovePosition::Bar => Ok(MovePosition::from_board_number(25 - dice_val).unwrap()),
            MovePosition::Off => Err(MoveError::SourceIsOff),
            _ => {
                let pos_num = self.board_number();
                if pos_num > dice_val {
                    let new_pos = pos_num - dice_val;
                    Ok(MovePosition::from_board_number(new_pos).unwrap())
                } else {
                    if allow_bear_off {
                        Ok(MovePosition::Off)
                    } else {
                        Err(MoveError::TargetOutOfBounds)
                    }
                }
            }
        }
    }
}

impl MovePosition {
    pub fn board_number(&self) -> u8 {
        match self {
            MovePosition::Bar => 25,
            MovePosition::Off => 0,
            MovePosition::One => 1,
            MovePosition::Two => 2,
            MovePosition::Three => 3,
            MovePosition::Four => 4,
            MovePosition::Five => 5,
            MovePosition::Six => 6,
            MovePosition::Seven => 7,
            MovePosition::Eight => 8,
            MovePosition::Nine => 9,
            MovePosition::Ten => 10,
            MovePosition::Eleven => 11,
            MovePosition::Twelve => 12,
            MovePosition::Thirteen => 13,
            MovePosition::Fourteen => 14,
            MovePosition::Fifteen => 15,
            MovePosition::Sixteen => 16,
            MovePosition::Seventeen => 17,
            MovePosition::Eighteen => 18,
            MovePosition::Nineteen => 19,
            MovePosition::Twenty => 20,
            MovePosition::TwentyOne => 21,
            MovePosition::TwentyTwo => 22,
            MovePosition::TwentyThree => 23,
            MovePosition::TwentyFour => 24,
        }
    }

    pub fn from_board_number(num: u8) -> Result<MovePosition, MoveError> {
        match num {
            1 => Ok(MovePosition::One),
            2 => Ok(MovePosition::Two),
            3 => Ok(MovePosition::Three),
            4 => Ok(MovePosition::Four),
            5 => Ok(MovePosition::Five),
            6 => Ok(MovePosition::Six),
            7 => Ok(MovePosition::Seven),
            8 => Ok(MovePosition::Eight),
            9 => Ok(MovePosition::Nine),
            10 => Ok(MovePosition::Ten),
            11 => Ok(MovePosition::Eleven),
            12 => Ok(MovePosition::Twelve),
            13 => Ok(MovePosition::Thirteen),
            14 => Ok(MovePosition::Fourteen),
            15 => Ok(MovePosition::Fifteen),
            16 => Ok(MovePosition::Sixteen),
            17 => Ok(MovePosition::Seventeen),
            18 => Ok(MovePosition::Eighteen),
            19 => Ok(MovePosition::Nineteen),
            20 => Ok(MovePosition::Twenty),
            21 => Ok(MovePosition::TwentyOne),
            22 => Ok(MovePosition::TwentyTwo),
            23 => Ok(MovePosition::TwentyThree),
            24 => Ok(MovePosition::TwentyFour),
            0 => Ok(MovePosition::Off),
            25 => Ok(MovePosition::Bar),
            _ => Err(MoveError::InvalidBoardNumber(num)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::piece_map::{PositionPieceMap, PositionPieceMapInner};

    #[test]
    fn opening_legal_moves_test() {
        let mut state = GameState::new();
        state.add_move(Move::Skip).unwrap();
        let roll = DiceRoll {
            dice_1: DiceValue::Three,
            dice_2: DiceValue::Two,
        };

        let move_1_res = state.add_move(Move::Plays((
            roll,
            vec![
                Play {
                    from: MovePosition::TwentyFour,
                    to: MovePosition::TwentyOne,
                    hit_status: HitStatus::NoHit,
                },
                Play {
                    from: MovePosition::Thirteen,
                    to: MovePosition::Eleven,
                    hit_status: HitStatus::NoHit,
                },
            ],
        )));

        assert!(move_1_res.is_ok());
        assert_eq!(
            state.p2_positions.piece_count_at(&MovePosition::Thirteen),
            4
        );
        assert_eq!(
            state.p2_positions.piece_count_at(&MovePosition::TwentyFour),
            1
        );
        assert_eq!(
            state.p2_positions.piece_count_at(&MovePosition::TwentyOne),
            1
        );
        assert_eq!(state.p2_positions.piece_count_at(&MovePosition::Eleven), 1);
    }

    #[test]
    fn can_only_move_from_bar_while_bar_is_occupied_test() {
        let mut two_pieces_in_bar_map = PositionPieceMapInner::new();
        two_pieces_in_bar_map.insert(MovePosition::Bar, 2);
        two_pieces_in_bar_map.insert(MovePosition::Eleven, 1);
        let state = GameState {
            past_moves: vec![],
            multiplier: 0,
            p1_positions: PositionPieceMap::from_inner(two_pieces_in_bar_map),
            p2_positions: PositionPieceMap::empty(),
            winner: None,
        };

        let roll = DiceRoll {
            dice_1: DiceValue::Three,
            dice_2: DiceValue::One,
        };

        for mov_seq in
            GameState::all_possible_moves(&roll, &state.p1_positions, &state.p2_positions)
        {
            for mov in mov_seq {
                assert_eq!(mov.from, MovePosition::Bar);
            }
        }
    }

    #[test]
    fn can_make_lesser_move_off_test() {
        let mut two_pieces_in_bar_map = PositionPieceMapInner::new();
        two_pieces_in_bar_map.insert(MovePosition::One, 2);

        let mut state = GameState {
            past_moves: vec![],
            multiplier: 1,
            p1_positions: PositionPieceMap::from_inner(two_pieces_in_bar_map),
            p2_positions: PositionPieceMap::empty(),
            winner: None,
        };

        let roll = DiceRoll {
            dice_1: DiceValue::Three,
            dice_2: DiceValue::Two,
        };

        for mov_seq in
            GameState::all_possible_moves(&roll, &state.p1_positions, &state.p2_positions)
        {
            for mov in mov_seq {
                assert_eq!(
                    mov,
                    Play {
                        from: MovePosition::One,
                        to: MovePosition::Off,
                        hit_status: HitStatus::NoHit
                    }
                );
            }
        }

        state.add_move(Move::Plays((roll, vec![Play {
            from: MovePosition::One,
            to: MovePosition::Off,
            hit_status: HitStatus::NoHit
        }, Play {
            from: MovePosition::One,
            to: MovePosition::Off,
            hit_status: HitStatus::NoHit
        }]))).unwrap();

        assert_eq!(state.winner, Some((Player::P1, 2)));
    }

    #[test]
    fn correct_positions_test() {
        let mut state = GameState::new();

        state.add_move(Move::Skip).unwrap();
        state
            .add_move(Move::Plays((
                DiceRoll {
                    dice_1: DiceValue::Three,
                    dice_2: DiceValue::Two,
                },
                vec![
                    Play {
                        hit_status: HitStatus::NoHit,
                        from: MovePosition::TwentyFour,
                        to: MovePosition::TwentyOne,
                    },
                    Play {
                        hit_status: HitStatus::NoHit,
                        from: MovePosition::Thirteen,
                        to: MovePosition::Eleven,
                    },
                ],
            )))
            .unwrap();
        state
            .add_move(Move::Plays((
                DiceRoll {
                    dice_1: DiceValue::Five,
                    dice_2: DiceValue::Two,
                },
                vec![
                    Play {
                        hit_status: HitStatus::NoHit,
                        from: MovePosition::Thirteen,
                        to: MovePosition::Eight,
                    },
                    Play {
                        hit_status: HitStatus::Hit,
                        from: MovePosition::Six,
                        to: MovePosition::Four,
                    },
                ],
            )))
            .unwrap();
        state
            .add_move(Move::Plays((
                DiceRoll {
                    dice_1: DiceValue::Five,
                    dice_2: DiceValue::Four,
                },
                vec![
                    Play {
                        hit_status: HitStatus::Hit,
                        from: MovePosition::Bar,
                        to: MovePosition::TwentyOne,
                    },
                    Play {
                        hit_status: HitStatus::NoHit,
                        from: MovePosition::Thirteen,
                        to: MovePosition::Eight,
                    },
                ],
            )))
            .unwrap();

        let mut expected_p1_positions = PositionPieceMapInner::new();
        expected_p1_positions.insert(MovePosition::TwentyFour, 2);
        expected_p1_positions.insert(MovePosition::Thirteen, 4);
        expected_p1_positions.insert(MovePosition::Six, 4);
        expected_p1_positions.insert(MovePosition::Eight, 4);
        expected_p1_positions.insert(MovePosition::Bar, 1);
        let mut expected_p2_positions = PositionPieceMapInner::new();
        expected_p2_positions.insert(MovePosition::TwentyFour, 1);
        expected_p2_positions.insert(MovePosition::TwentyOne, 1);
        expected_p2_positions.insert(MovePosition::Eleven, 1);
        expected_p2_positions.insert(MovePosition::Thirteen, 3);
        expected_p2_positions.insert(MovePosition::Six, 5);
        expected_p2_positions.insert(MovePosition::Eight, 4);

        assert_eq!(
            PositionPieceMap::from_inner(expected_p1_positions),
            state.p1_positions
        );
        assert_eq!(
            PositionPieceMap::from_inner(expected_p2_positions),
            state.p2_positions
        );
    }

    #[test]
    fn account_for_intermediary_moves_test() {
        let mut map = PositionPieceMapInner::new();
        map.insert(MovePosition::Twenty, 2);
        map.insert(MovePosition::Eighteen, 1);

        let state = GameState {
            past_moves: vec![],
            multiplier: 0,
            p1_positions: PositionPieceMap::from_inner(map),
            p2_positions: PositionPieceMap::empty(),
            winner: None,
        };

        let roll = DiceRoll {
            dice_1: DiceValue::Six,
            dice_2: DiceValue::Six,
        };
        let validation_result = state.validate_play(
            &roll,
            &vec![
                Play {
                    hit_status: HitStatus::NoHit,
                    from: MovePosition::Twenty,
                    to: MovePosition::Fourteen,
                },
                Play {
                    hit_status: HitStatus::NoHit,
                    from: MovePosition::Twenty,
                    to: MovePosition::Fourteen,
                },
                Play {
                    hit_status: HitStatus::NoHit,
                    from: MovePosition::Eighteen,
                    to: MovePosition::Twelve,
                },
                // we initially didn't have any piece at 14
                Play {
                    hit_status: HitStatus::NoHit,
                    from: MovePosition::Fourteen,
                    to: MovePosition::Eight,
                },
            ],
        );

        assert_eq!(validation_result, Ok(()));
    }

    #[test]
    fn less_than_4_moves_possible_test() {
        let mut map = PositionPieceMapInner::new();
        map.insert(MovePosition::Four, 2);
        map.insert(MovePosition::Five, 2);
        map.insert(MovePosition::Six, 2);

        let mut opp_map = PositionPieceMapInner::new();
        opp_map.insert(MovePosition::TwentyFour, 2);

        let state = GameState {
            past_moves: vec![],
            multiplier: 0,
            p1_positions: PositionPieceMap::from_inner(map),
            p2_positions: PositionPieceMap::from_inner(opp_map),
            winner: None,
        };

        let roll = DiceRoll {
            dice_1: DiceValue::Five,
            dice_2: DiceValue::Five,
        };
        let validation_result = state.validate_play(
            &roll,
            &vec![
                Play {
                    hit_status: HitStatus::NoHit,
                    from: MovePosition::Five,
                    to: MovePosition::Off,
                },
                Play {
                    hit_status: HitStatus::NoHit,
                    from: MovePosition::Five,
                    to: MovePosition::Off,
                },
            ],
        );

        assert_eq!(validation_result, Ok(()));
    }

    #[test]
    fn response_legal_moves_test() {
        let mut state = GameState::new();
        assert!(state.add_move(Move::Skip).is_ok());
        let roll = DiceRoll {
            dice_1: DiceValue::Three,
            dice_2: DiceValue::Two,
        };

        assert!(state
            .add_move(Move::Plays((
                roll,
                vec![
                    Play {
                        from: MovePosition::TwentyFour,
                        to: MovePosition::TwentyOne,
                        hit_status: HitStatus::NoHit
                    },
                    Play {
                        from: MovePosition::Thirteen,
                        to: MovePosition::Eleven,
                        hit_status: HitStatus::NoHit
                    }
                ]
            )))
            .is_ok());

        let roll = DiceRoll {
            dice_1: DiceValue::Five,
            dice_2: DiceValue::Two,
        };

        let move_2_res = state.add_move(Move::Plays((
            roll,
            vec![
                Play {
                    from: MovePosition::Thirteen,
                    to: MovePosition::Eight,
                    hit_status: HitStatus::NoHit,
                },
                Play {
                    from: MovePosition::Six,
                    to: MovePosition::Four,
                    hit_status: HitStatus::Hit,
                },
            ],
        )));
        assert!(move_2_res.is_ok());

        let roll = DiceRoll {
            dice_1: DiceValue::Five,
            dice_2: DiceValue::Four,
        };

        let move_3_res = state.add_move(Move::Plays((
            roll,
            vec![
                Play {
                    from: MovePosition::Bar,
                    to: MovePosition::TwentyOne,
                    hit_status: HitStatus::Hit,
                },
                Play {
                    from: MovePosition::Thirteen,
                    to: MovePosition::Eight,
                    hit_status: HitStatus::NoHit,
                },
            ],
        )));
        assert!(move_3_res.is_ok());
    }
}
