use crate::Player::P1;
use micromap::Map;
use std::array::IntoIter;
use std::cmp::PartialEq;
use std::iter::{Filter, FlatMap};
use std::num::ParseIntError;
use std::rc::Rc;
use std::slice::Iter;

pub fn add(left: u64, right: u64) -> u64 {
    left + right
}

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
        return match self {
            DiceValue::One => 1,
            DiceValue::Two => 2,
            DiceValue::Three => 3,
            DiceValue::Four => 4,
            DiceValue::Five => 5,
            DiceValue::Six => 6,
        };
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

#[derive(Clone, Debug)]
pub enum ParsingErrorType {
    NoMoveNumber,
    MoveNotInteger(ParseIntError),
    MoveError(MoveError),
    NoDice,
    DiceNotInteger,
    InvalidDice,
    InvalidPlay,
    InvalidPosition,
    InvalidPlayEnd,
    InvalidPlayStart,
    InvalidDiceSeparator,
    InvalidDealMove,
    UnexpectedLine,
    EmptyString,
}

#[derive(Clone, Debug)]
pub struct ParsingError {
    pub error_type: ParsingErrorType,
    pub line_no: usize,
    pub line_pos: usize,
}

impl ParsingError {
    pub fn from_move_error(move_error: MoveError, line_no: usize) -> ParsingError {
        ParsingError {
            error_type: ParsingErrorType::MoveError(move_error),
            line_no,
            line_pos: 0,
        }
    }
}

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

    pub fn parse(source_line: &str, line_no: usize) -> Result<MoveTuple, ParsingError> {
        let move_no_separator_start_location =
            search_forwards_until_match(source_line, 0, |c| c == ')').ok_or(ParsingError {
                error_type: ParsingErrorType::NoMoveNumber,
                line_no: line_no,
                line_pos: 0,
            })?;

        let lookup_idx = move_no_separator_start_location - 1;
        let start_pos =
            search_backwards_until_match(source_line, lookup_idx, |c| c.is_ascii_whitespace())
                .map(|ws_idx| ws_idx + 1)
                .unwrap_or(0);

        let move_no = collect_chars(
            source_line,
            start_pos,
            Some(move_no_separator_start_location),
        )
        .parse::<usize>()
        .map_err(|e| ParsingError {
            error_type: ParsingErrorType::MoveNotInteger(e),
            line_no: line_no,
            line_pos: start_pos,
        })?;

        let d1 = parse_next_dice(source_line, move_no_separator_start_location + 1, line_no).ok();
        match d1 {
            Some(d1) => {
                match parse_next_dice(source_line, d1.start_idx + 3, line_no) {
                    Ok(d2) => {
                        let d1_moves_start = d1.start_idx + 3;
                        let d1_moves_end = d2.start_idx;

                        let p1_moves =
                            collect_chars(source_line, d1_moves_start, Some(d1_moves_end));
                        let p1_moves = parse_moves(
                            &p1_moves,
                            line_no,
                            DiceRoll {
                                dice_1: DiceValue::try_from(d1.dice_1).unwrap(),
                                dice_2: DiceValue::try_from(d1.dice_2).unwrap(),
                            },
                        )?;

                        let mut mt = MoveTuple {
                            move_1: Some(p1_moves),
                            move_2: None,
                            move_no: move_no,
                        };

                        // parse d2 moves

                        let d2_moves_start = d2.start_idx + 3;
                        let p2_moves = collect_chars(source_line, d2_moves_start, None);
                        mt.move_2 = Some(parse_moves(
                            &p2_moves,
                            line_no,
                            DiceRoll {
                                dice_1: DiceValue::try_from(d2.dice_1).unwrap(),
                                dice_2: DiceValue::try_from(d2.dice_2).unwrap(),
                            },
                        )?);

                        Ok(mt)
                    }
                    Err(_) => {
                        // only one dice. this means it's either:
                        // 1. takes followed by dice
                        // 2. dice followed by doubles
                        // 3. dice followed by nothing
                        let collected =
                            collect_chars(source_line, move_no_separator_start_location + 1, None);
                        let collected_split: Vec<&str> =
                            collected.split_ascii_whitespace().collect();
                        match *collected_split.first().unwrap() {
                            "Takes" => {
                                // 1. takes followed by dice

                                let d1_moves_start = d1.start_idx + 3;
                                let p1_moves = collect_chars(source_line, d1_moves_start, None);
                                let p1_moves = parse_moves(
                                    &p1_moves,
                                    line_no,
                                    DiceRoll {
                                        dice_1: DiceValue::try_from(d1.dice_1).unwrap(),
                                        dice_2: DiceValue::try_from(d1.dice_2).unwrap(),
                                    },
                                )?;

                                Ok(MoveTuple {
                                    move_1: Some(Move::Take),
                                    move_2: Some(p1_moves),
                                    move_no: move_no,
                                })
                            }
                            _ => {
                                if collected_split.len() >= 3 {
                                    // 2. dice followed by doubles
                                    let parse_error = ParsingError {
                                        line_no: line_no,
                                        line_pos: move_no_separator_start_location + 1,
                                        error_type: ParsingErrorType::InvalidDealMove,
                                    };

                                    let first_item = collected_split
                                        .get(collected_split.len() - 3)
                                        .ok_or(parse_error.clone())?;
                                    let second_item = collected_split
                                        .get(collected_split.len() - 2)
                                        .ok_or(parse_error.clone())?;

                                    if *first_item == "Doubles" && *second_item == "=>" {
                                        let third_item =
                                            collected_split.last().ok_or(parse_error.clone())?;
                                        let multiplier: usize =
                                            third_item.parse().ok().ok_or(parse_error.clone())?;
                                        // we now know the doubles value. parse dice play and return

                                        let d1_moves_start = d1.start_idx + 3;
                                        let d1_moves_end = search_forwards_until_match(
                                            source_line,
                                            d1.start_idx + 3,
                                            |c| c == 'D',
                                        )
                                        .unwrap();
                                        let p1_moves = collect_chars(
                                            source_line,
                                            d1_moves_start,
                                            Some(d1_moves_end),
                                        );
                                        let p1_moves = parse_moves(
                                            &p1_moves,
                                            line_no,
                                            DiceRoll {
                                                dice_1: DiceValue::try_from(d1.dice_1).unwrap(),
                                                dice_2: DiceValue::try_from(d1.dice_2).unwrap(),
                                            },
                                        )?;

                                        return Ok(MoveTuple {
                                            move_1: Some(p1_moves),
                                            move_2: Some(Move::Double(multiplier)),
                                            move_no: move_no,
                                        });
                                    }
                                }

                                // 3. dice followed by nothing
                                let d1_moves_start = d1.start_idx + 3;
                                let p1_moves = collect_chars(source_line, d1_moves_start, None);
                                let p1_moves = parse_moves(
                                    &p1_moves,
                                    line_no,
                                    DiceRoll {
                                        dice_1: DiceValue::try_from(d1.dice_1).unwrap(),
                                        dice_2: DiceValue::try_from(d1.dice_2).unwrap(),
                                    },
                                )?;

                                Ok(MoveTuple {
                                    move_1: Some(p1_moves),
                                    move_2: None,
                                    move_no: move_no,
                                })
                            }
                        }
                    }
                }
            }
            None => {
                let collected =
                    collect_chars(source_line, move_no_separator_start_location + 1, None);
                let collected_split: Vec<&str> = collected.split_ascii_whitespace().collect();

                let parse_error = ParsingError {
                    line_no: line_no,
                    line_pos: move_no_separator_start_location + 1,
                    error_type: ParsingErrorType::InvalidDealMove,
                };

                let split_len = collected_split.len();

                match split_len {
                    1 => match *collected_split.get(0).unwrap() {
                        "Takes" => Ok(MoveTuple {
                            move_1: Some(Move::Take),
                            move_2: None,
                            move_no: move_no,
                        }),
                        "Drops" => Ok(MoveTuple {
                            move_1: Some(Move::Drop),
                            move_2: None,
                            move_no: move_no,
                        }),
                        _ => Err(parse_error),
                    },
                    3 => {
                        let third_item = collected_split.get(2).ok_or(parse_error.clone())?;
                        let multiplier: usize =
                            third_item.parse().ok().ok_or(parse_error.clone())?;
                        let first_item = collected_split.get(0).ok_or(parse_error.clone())?;
                        let second_item = collected_split.get(1).ok_or(parse_error.clone())?;

                        if *first_item != "Doubles" || *second_item != "=>" {
                            Err(parse_error.clone())
                        } else {
                            Ok(MoveTuple {
                                move_1: Some(Move::Double(multiplier)),
                                move_2: None,
                                move_no: move_no,
                            })
                        }
                    }
                    4 => {
                        let third_item = collected_split.get(2).ok_or(parse_error.clone())?;
                        let multiplier: usize =
                            third_item.parse().ok().ok_or(parse_error.clone())?;
                        let first_item = collected_split.get(0).ok_or(parse_error.clone())?;
                        let second_item = collected_split.get(1).ok_or(parse_error.clone())?;

                        if *first_item != "Doubles" || *second_item != "=>" {
                            Err(parse_error.clone())
                        } else {
                            match *collected_split.get(3).unwrap() {
                                "Takes" => Ok(MoveTuple {
                                    move_1: Some(Move::Double(multiplier)),
                                    move_2: Some(Move::Take),
                                    move_no: move_no,
                                }),
                                "Drops" => Ok(MoveTuple {
                                    move_1: Some(Move::Double(multiplier)),
                                    move_2: Some(Move::Drop),
                                    move_no: move_no,
                                }),
                                _ => Err(parse_error),
                            }
                        }
                    }
                    _ => Err(parse_error),
                }
            }
        }
    }
}

fn parse_moves(line: &str, line_no: usize, dice_roll: DiceRoll) -> Result<Move, ParsingError> {
    let mut plays: Vec<Play> = Vec::new();
    let split = line.trim().split_ascii_whitespace();

    for play_candidate in split {
        let ends_critical = play_candidate.ends_with("*");
        let valid_ending = ends_critical
            || (play_candidate.chars().last().is_some()
                && play_candidate.chars().last().unwrap().is_ascii_digit());

        if !valid_ending {
            return Err(ParsingError {
                error_type: ParsingErrorType::InvalidPlayEnd,
                line_no: line_no,
                line_pos: 0,
            });
        }

        let valid_start = play_candidate.chars().nth(0).is_some()
            && play_candidate.chars().nth(0).unwrap().is_ascii_digit();

        if !valid_start {
            return Err(ParsingError {
                error_type: ParsingErrorType::InvalidPlayStart,
                line_no: line_no,
                line_pos: 0,
            });
        }

        let mut source_target_split = play_candidate.split("/");

        let source_str = source_target_split.next();
        let target_str = source_target_split.next();

        if source_target_split.next().is_some() || source_str.is_none() || target_str.is_none() {
            return Err(ParsingError {
                error_type: ParsingErrorType::InvalidPlay,
                line_no: line_no,
                line_pos: 0,
            });
        }

        let source_int: Option<u8> = source_str.unwrap().parse().ok();
        let target_int: Option<u8> = if ends_critical {
            let mut target_owned = target_str.unwrap().to_owned();
            target_owned.truncate(target_owned.len() - 1);
            target_owned.parse().ok()
        } else {
            target_str.unwrap().parse().ok()
        };

        for i in [source_int, target_int] {
            if i.is_none() || i.unwrap() > 25 {
                return Err(ParsingError {
                    error_type: ParsingErrorType::InvalidPosition,
                    line_no: line_no,
                    line_pos: 0,
                });
            }
        }

        plays.push(Play {
            hit_status: if ends_critical {
                HitStatus::Hit
            } else {
                HitStatus::NoHit
            },
            from: MovePosition::from_board_number(source_int.unwrap()).unwrap(),
            to: MovePosition::from_board_number(target_int.unwrap()).unwrap(),
        });
    }
    if plays.is_empty() {
        Ok(Move::CantPlay(dice_roll))
    } else {
        Ok(Move::Plays((dice_roll, plays)))
    }
}

fn parse_next_dice(
    line: &str,
    start_idx: usize,
    line_no: usize,
) -> Result<ParsedDice, ParsingError> {
    let move_separator_address =
        search_forwards_until_match(line, start_idx, |c| c == ':').ok_or(ParsingError {
            error_type: ParsingErrorType::NoDice,
            line_no: line_no,
            line_pos: 0,
        })?;
    let dice_1_val_address = move_separator_address - 2;

    let dice_1 = check_dice_at_address(line, line_no, dice_1_val_address)?;

    let dice_2_val_address = move_separator_address - 1;

    let dice_2 = check_dice_at_address(line, line_no, dice_2_val_address)?;

    let parse_err = ParsingError {
        error_type: ParsingErrorType::InvalidDiceSeparator,
        line_no: line_no,
        line_pos: dice_2_val_address,
    };
    let move_separator_check = line
        .chars()
        .nth(move_separator_address)
        .map(|c| {
            if c == ':' {
                Ok(())
            } else {
                Err(parse_err.clone())
            }
        })
        .ok_or(parse_err)
        .and_then(|f| f);
    move_separator_check?;

    Ok(ParsedDice {
        start_idx: dice_1_val_address,
        dice_1,
        dice_2,
    })
}

struct ParsedDice {
    start_idx: usize,
    dice_1: u8,
    dice_2: u8,
}

fn check_dice_at_address(
    source_line: &str,
    line_no: usize,
    address: usize,
) -> Result<u8, ParsingError> {
    let dice_2_check: Result<u8, ParsingError> = source_line
        .chars()
        .nth(address)
        .ok_or(ParsingError {
            error_type: ParsingErrorType::NoDice,
            line_no: line_no,
            line_pos: address,
        })
        .and_then(|c| {
            is_valid_dice(&c).ok_or(ParsingError {
                error_type: ParsingErrorType::DiceNotInteger,
                line_no: line_no,
                line_pos: address,
            })
        });

    dice_2_check
}

fn is_valid_dice(char: &char) -> Option<u8> {
    if !char.is_ascii_digit() {
        return None;
    }

    let mut s = String::new();
    s.push(char.clone());
    let int = s.parse::<u8>().unwrap();
    if int >= 1 && int <= 6 {
        return Some(int);
    } else {
        None
    }
}
fn search_backwards_until_match<F>(line: &str, start_idx: usize, matcher: F) -> Option<usize>
where
    F: FnOnce(char) -> bool + Copy,
{
    let mut idx = start_idx;

    while let Some(char) = line.chars().nth(idx) {
        let value = matcher(char);
        if value {
            return Some(idx);
        }
        if idx == 0 {
            return None;
        }
        idx -= 1;
    }

    None
}

fn search_forwards_until_match<F>(line: &str, start_idx: usize, matcher: F) -> Option<usize>
where
    F: FnOnce(char) -> bool + Copy,
{
    let mut idx = start_idx;

    while let Some(char) = line.chars().nth(idx) {
        let value = matcher(char);
        if value {
            return Some(idx);
        }
        idx += 1;
    }

    None
}

fn collect_chars(line: &str, from_inc: usize, to_exc: Option<usize>) -> String {
    let mut collector = String::new();
    let mut idx = from_inc;
    while let Some(char) = line.chars().nth(idx) {
        if to_exc.is_some() && idx >= to_exc.unwrap() {
            break;
        }
        collector.push(char);
        idx += 1;
    }

    collector
}

#[derive(Clone, Copy, PartialEq)]
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

#[derive(Debug, Clone)]
pub enum MoveError {
    ResponseWithoutDoubleOffer,
    HaveToRespond,
    PlayPossible,
    InvalidBoardNumber(u8),
    InvalidPlayCount,
    TargetOutOfBounds,
    TargetIsBar,
    SourceIsOff,
    NoPieceInSource,
    TargetOccupied,
    IllegalBearOff,
    IllegalDouble,
    PlayDiceMismatch,
}

type PositionPieceMap = Map<MovePosition, u8, 26>;

pub struct GameState {
    pub past_moves: Vec<MoveTuple>,
    pub multiplier: usize,
    pub p1_positions: PositionPieceMap,
    pub p2_positions: PositionPieceMap,
    pub winner: Option<(Player, usize)>,
}

impl GameState {
    pub fn new() -> GameState {
        let mut pos_map = PositionPieceMap::new();
        pos_map.insert(MovePosition::TwentyFour, 2);
        pos_map.insert(MovePosition::Thirteen, 5);
        pos_map.insert(MovePosition::Eight, 3);
        pos_map.insert(MovePosition::Six, 5);

        GameState {
            past_moves: vec![],
            multiplier: 1,
            p1_positions: pos_map.clone(),
            p2_positions: pos_map,
            winner: None,
        }
    }

    pub fn add_move(&mut self, candidate: Move) -> Result<(), MoveError> {
        self.validate_answer_issues(&candidate)?;
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

    fn apply_move(&mut self, mov: Move, side: &Player) {
        let mut winner: Option<(Player, usize)> = None;
        match &mov {
            Move::Plays((_, plays)) => {
                let mut pp_mut = self.get_player_positions(side).clone();
                let mut opp_mut = self.get_player_positions(&side.other()).clone();
                for play in plays {
                    let p1: Vec<String> = pp_mut
                        .iter()
                        .filter(|(k, v)| **v >= 1)
                        .map(|(k, v)| format!("{:?}, {:?}", k.board_number(), v))
                        .collect();
                    let p2: Vec<String> = opp_mut
                        .iter()
                        .filter(|(k, v)| **v >= 1)
                        .map(|(k, v)| format!("{:?}, {:?}", k.board_number(), v))
                        .collect();
                    assert!(Self::make_play_mutating(
                        &play.from,
                        &play.to,
                        &mut pp_mut,
                        &mut opp_mut
                    )
                    .is_ok());
                }

                let p1: Vec<String> = pp_mut
                    .iter()
                    .filter(|(k, v)| **v >= 1)
                    .map(|(k, v)| format!("{:?}, {:?}", k.board_number(), v))
                    .collect();
                let p2: Vec<String> = opp_mut
                    .iter()
                    .filter(|(k, v)| **v >= 1)
                    .map(|(k, v)| format!("{:?}, {:?}", k.board_number(), v))
                    .collect();
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
                // todo: check for wins
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

        let matches_any_known_play = Self::all_possible_moves(
            dice_roll,
            self.get_player_positions(&side),
            self.get_player_positions(&side.other()),
        )
        .any(|mut sequence_iterator| {
            let seq_collected: Vec<Play> = sequence_iterator.collect();

            seq_collected.iter().eq(plays)
        });

        if !matches_any_known_play {
            Err(MoveError::PlayDiceMismatch)
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
        Self::can_move(from, to, &player_pos_map, &opp_pos_map)?;

        let old_player_pos_val = player_pos_map.get(from).unwrap();
        let new_player_pos_val = old_player_pos_val - 1;
        player_pos_map.insert(*from, new_player_pos_val);

        if let Some(to_converted_to_opp_coordinates) = to.translate_to_opponent_pov() {
            let v = opp_pos_map.remove(&to_converted_to_opp_coordinates);
            if let Some(v) = v {
                let old_player_pos_val = opp_pos_map.get(&MovePosition::Bar).unwrap_or(&0);
                let new_player_pos_val = old_player_pos_val + 1;
                opp_pos_map.insert(MovePosition::Bar, new_player_pos_val);
            }
        }

        let old_player_to_val = player_pos_map.get(to).unwrap_or(&0);
        let new_player_to_val = old_player_to_val + 1;
        player_pos_map.insert(*to, new_player_to_val);

        Ok(())
    }

    fn validate_answer_issues(&self, candidate: &Move) -> Result<(), MoveError> {
        let last_move = self.last_move();

        let have_to_answer = match last_move {
            Some(m) => match m {
                Move::Double(next_multiplier) => {
                    if *next_multiplier == self.multiplier * 2 {
                        true
                    } else {
                        return Err(MoveError::IllegalDouble);
                    }
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
                    if Self::can_move(&from, &to, &state.0, &state.1).is_ok() {
                        let mut new_player_map = state.0.clone();
                        let mut new_opp_map = state.1.clone();

                        let to_opponent_value = to.translate_to_opponent_pov();
                        let is_hit = to_opponent_value
                            .and_then(|pos| new_opp_map.get(&pos))
                            .map(|c| *c == 1)
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
    ) -> Result<(), MoveError> {
        match from {
            MovePosition::Off => Err(MoveError::SourceIsOff),
            _ => {
                if player_pos_map.get(from).cloned().unwrap_or_default() < 1 {
                    Err(MoveError::NoPieceInSource)
                } else {
                    match to {
                        MovePosition::Bar => Err(MoveError::TargetIsBar),
                        MovePosition::Off => {
                            if !Self::is_bear_off_eligible(player_pos_map) {
                                Err(MoveError::IllegalBearOff)
                            } else {
                                Ok(())
                            }
                        }
                        _ => {
                            let to_opp_coord = to.translate_to_opponent_pov().unwrap();
                            if opp_pos_map.get(&to_opp_coord).cloned().unwrap_or_default() > 1 {
                                Err(MoveError::TargetOccupied)
                            } else {
                                Ok(())
                            }
                        }
                    }
                }
            }
        }
    }

    pub fn is_bear_off_eligible(pos_map: &PositionPieceMap) -> bool {
        if pos_map.get(&MovePosition::Bar).cloned().unwrap_or_default() > 0 {
            return false;
        }
        pos_map
            .iter()
            .filter(|(p, c)| **c > 0 && **p != MovePosition::Off)
            .all(|(p, _)| p.board_number().unwrap() <= 6)
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

    fn build_parsing(
        &mut self,
        res: GameParseFirstLineResult,
        line_no: usize,
    ) -> Result<(), ParsingError> {
        match res {
            GameParseFirstLineResult::NsTuple(nst) => {
                if self.p1_name.is_some()
                    || self.p2_name.is_some()
                    || self.p1_points.is_some()
                    || self.p2_points.is_some()
                {
                    Err(ParsingError {
                        error_type: ParsingErrorType::UnexpectedLine,
                        line_no,
                        line_pos: 0,
                    })
                } else {
                    self.p1_name = Some(nst.0.name);
                    self.p2_name = Some(nst.1.name);
                    self.p1_points = Some(nst.0.score);
                    self.p2_points = Some(nst.1.score);
                    Ok(())
                }
            }
            GameParseFirstLineResult::MoveTuple(mt) => {
                if self.state.past_moves.len() == 1
                    && self.state.past_moves.last().unwrap().move_2.is_none()
                {
                    // apply corrections TODO: remove this hack after implementing proper single move parsing
                    let new_first_move = self.state.past_moves.pop().unwrap().move_1.unwrap();
                    let mut new_state = GameState::new();
                    new_state
                        .add_move(Move::Skip)
                        .map_err(|ge| ParsingError::from_move_error(ge, line_no))?;
                    new_state
                        .add_move(new_first_move)
                        .map_err(|ge| ParsingError::from_move_error(ge, line_no))?;
                    self.state = new_state;
                }

                for mov in mt.moves().into_iter() {
                    self.state
                        .add_move(mov.clone())
                        .map_err(|ge| ParsingError::from_move_error(ge, line_no))?;
                }

                Ok(())
            }
            GameParseFirstLineResult::GameName(gn) => {
                if self.name.is_some() {
                    Err(ParsingError {
                        error_type: ParsingErrorType::UnexpectedLine,
                        line_no,
                        line_pos: 0,
                    })
                } else {
                    self.name = Some(gn);
                    Ok(())
                }
            }
        }
    }

    pub fn parse(source_lines: Vec<&str>, start_line_idx: usize) -> Result<Game, ParsingError> {
        match source_lines.len() {
            0 => Err(ParsingError {
                error_type: ParsingErrorType::EmptyString,
                line_no: start_line_idx,
                line_pos: 0,
            }),
            _ => {
                let mut g = Game::new();
                for (i, line) in source_lines.iter().enumerate() {
                    let idx = start_line_idx + i;
                    let parsed = parse_first_line(line, idx)?;
                    let err = g.build_parsing(parsed, idx);
                    err?;
                }

                Ok(g)
            }
        }
    }
}

enum GameParseFirstLineResult {
    NsTuple((NameScoreTuple, NameScoreTuple)),
    MoveTuple(MoveTuple),
    GameName(String),
}

fn parse_first_line(line: &str, line_no: usize) -> Result<GameParseFirstLineResult, ParsingError> {
    if let Ok(mt) = MoveTuple::parse(line, line_no) {
        return Ok(GameParseFirstLineResult::MoveTuple(mt));
    }

    if let Ok(sb) = parse_scoreline(line, line_no) {
        return Ok(GameParseFirstLineResult::NsTuple(sb));
    }

    Ok(GameParseFirstLineResult::GameName(line.to_owned()))
}

struct NameScoreTuple {
    name: String,
    score: usize,
}

fn parse_scoreline(
    line: &str,
    line_no: usize,
) -> Result<(NameScoreTuple, NameScoreTuple), ParsingError> {
    let split: Vec<&str> = line.split_whitespace().collect();
    if split.len() != 6 {
        Err(ParsingError {
            error_type: ParsingErrorType::UnexpectedLine,
            line_no,
            line_pos: 0,
        })?
    }

    if split[1] != split[4] || split[1] != ":" {
        Err(ParsingError {
            error_type: ParsingErrorType::UnexpectedLine,
            line_no,
            line_pos: 0,
        })?
    }

    let score_1_parsed: usize = split[2].parse().map_err(|e| ParsingError {
        error_type: ParsingErrorType::UnexpectedLine,
        line_no,
        line_pos: 0,
    })?;

    let score_2_parsed: usize = split[5].parse().map_err(|e| ParsingError {
        error_type: ParsingErrorType::UnexpectedLine,
        line_no,
        line_pos: 0,
    })?;

    Ok((
        NameScoreTuple {
            name: split[0].to_string(),
            score: score_1_parsed,
        },
        NameScoreTuple {
            score: score_2_parsed,
            name: split[3].to_string(),
        },
    ))
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
        let num = self.board_number().unwrap();
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
                let pos_num = self.board_number().unwrap();
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
    pub fn board_number(&self) -> Option<u8> {
        return match self {
            MovePosition::Bar => Some(25),
            MovePosition::Off => Some(0),
            MovePosition::One => Some(1),
            MovePosition::Two => Some(2),
            MovePosition::Three => Some(3),
            MovePosition::Four => Some(4),
            MovePosition::Five => Some(5),
            MovePosition::Six => Some(6),
            MovePosition::Seven => Some(7),
            MovePosition::Eight => Some(8),
            MovePosition::Nine => Some(9),
            MovePosition::Ten => Some(10),
            MovePosition::Eleven => Some(11),
            MovePosition::Twelve => Some(12),
            MovePosition::Thirteen => Some(13),
            MovePosition::Fourteen => Some(14),
            MovePosition::Fifteen => Some(15),
            MovePosition::Sixteen => Some(16),
            MovePosition::Seventeen => Some(17),
            MovePosition::Eighteen => Some(18),
            MovePosition::Nineteen => Some(19),
            MovePosition::Twenty => Some(20),
            MovePosition::TwentyOne => Some(21),
            MovePosition::TwentyTwo => Some(22),
            MovePosition::TwentyThree => Some(23),
            MovePosition::TwentyFour => Some(24),
        };
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

    #[test]
    fn parse_game_ex() {
        let game = "Game 1
 Mochizuki_Masayouki_(JPN) : 0  Takumitsu_Suzuki_(JPN) : 0
  1)                             32: 24/21 13/11
  2) 52: 13/8 6/4*               54: 25/21* 13/8
  3) 43: 25/21 24/21             53: 24/21 13/8
  4) 62: 13/7 7/5                64: 21/15 15/11
  5) 44: 13/9 9/5 8/4* 8/4       21: 25/23 23/22
  6)  Doubles => 2                Takes
  7) 21: 6/4 4/3*                32: 25/22* 8/6
  8) 32: 25/23 6/3*              21: 25/23 23/22*
  9) 54: 25/20 24/20             33: 13/10 13/10 6/3 6/3
 10) 62: 13/7 7/5                53: 11/8 11/6
 11) 22: 23/21 13/11 11/9 5/3*   21: 25/23 23/22*
 12) 31: 25/24 21/18             54: 22/18 18/13
 13) 66: 20/14 20/14 18/12* 14/8 44:
 14) 64: 14/10 12/6              54:
 15) 31: 10/7 8/7                51: 25/24 8/3
 16) 42: 24/20 20/18             33: 10/7* 10/7 8/5 8/5
 17) 33:                         66: 8/2 8/2 7/1 7/1
 18) 51:                         64: 6/2
 19) 54: 25/21 21/16             61: 24/23
 20) 53: 21/18 21/16             21: 23/22 6/4
 21) 43: 16/12 12/9              31: 6/5 4/1
 22) 31: 16/15 6/3*              55:
 23) 64: 9/5 9/3                 62: 25/23
 24) 54: 15/10 10/6              65: 6/1
 25) 54: 18/13 6/2*              22: 25/23* 6/4 5/3 4/2
 26) 22:                         64: 5/1
 27) 63: 25/19 5/2*              41: 25/24 5/1
 28) 33: 13/10 10/7 8/5 5/2      55:
 29) 54: 19/14 14/10             62: 3/1
 30) 21: 10/9 8/6                42: 3/1
 31) 61: 9/8 7/1*                64:
 32) 33: 8/5 7/4 7/4 4/1         61:
 33) 61: 6/0 5/4                 33:
 34) 52: 6/4 6/1                 62: 25/19 19/17
 35) 32: 3/0 2/0                 61: 17/11 11/10
 36) 11: 4/3 1/0 1/0 1/0         31: 10/7 7/6
 37) 63: 5/0 3/0                 33: 6/3 3/0 3/0 3/0
 38) 62: 5/0 2/0                 61: 2/0 1/0
 39) 11: 4/3 3/2 2/1 1/0         65: 2/0 2/0
 40) 61: 4/0 3/2                 64: 2/0 1/0
 41) 61: 4/0 2/1                 51: 1/0 1/0
 42) 22: 4/2 2/0 1/0
      Wins 2 points";

        let lines = game.lines().collect();
        let parsed = Game::parse(lines, 0);
        assert!(parsed.is_ok());
    }

    #[test]
    fn move_tuple_standard() {
        let str = "18) 43: 25/21 13/10             53: 6/3 6/1 ";
        let parsed = MoveTuple::parse(str, 1);
        assert!(parsed.is_ok());
        let parsed = parsed.unwrap();
        assert_eq!(parsed.move_no, 18);
        assert_eq!(
            parsed.move_1,
            Some(Move::Plays((
                DiceRoll {
                    dice_1: DiceValue::Four,
                    dice_2: DiceValue::Three
                },
                vec![
                    Play {
                        hit_status: HitStatus::NoHit,
                        from: MovePosition::Bar,
                        to: MovePosition::TwentyOne
                    },
                    Play {
                        hit_status: HitStatus::NoHit,
                        from: MovePosition::Thirteen,
                        to: MovePosition::Ten
                    },
                ]
            )))
        );
        assert_eq!(
            parsed.move_2,
            Some(Move::Plays((
                DiceRoll {
                    dice_1: DiceValue::Five,
                    dice_2: DiceValue::Three
                },
                vec![
                    Play {
                        hit_status: HitStatus::NoHit,
                        from: MovePosition::Six,
                        to: MovePosition::Three
                    },
                    Play {
                        hit_status: HitStatus::NoHit,
                        from: MovePosition::Six,
                        to: MovePosition::One
                    },
                ]
            )))
        );
    }

    #[test]
    fn move_tuple_hit() {
        let str = "2) 52: 13/8 6/4*               54: 25/21* 13/8";
        let parsed = MoveTuple::parse(str, 1);
        assert!(parsed.is_ok());
        let parsed = parsed.unwrap();
        assert_eq!(parsed.move_no, 2);
        assert_eq!(
            parsed.move_1,
            Some(Move::Plays((
                DiceRoll {
                    dice_1: DiceValue::Five,
                    dice_2: DiceValue::Two
                },
                vec![
                    Play {
                        hit_status: HitStatus::NoHit,
                        from: MovePosition::Thirteen,
                        to: MovePosition::Eight
                    },
                    Play {
                        hit_status: HitStatus::Hit,
                        from: MovePosition::Six,
                        to: MovePosition::Four
                    },
                ]
            )))
        );
        assert_eq!(
            parsed.move_2,
            Some(Move::Plays((
                DiceRoll {
                    dice_1: DiceValue::Five,
                    dice_2: DiceValue::Four
                },
                vec![
                    Play {
                        hit_status: HitStatus::Hit,
                        from: MovePosition::Bar,
                        to: MovePosition::TwentyOne
                    },
                    Play {
                        hit_status: HitStatus::NoHit,
                        from: MovePosition::Thirteen,
                        to: MovePosition::Eight
                    },
                ]
            )))
        );
    }

    #[test]
    fn move_tuple_doubles_takes() {
        let str = "  6)  Doubles => 2                Takes";
        let parsed = MoveTuple::parse(str, 1);
        assert!(parsed.is_ok());
        let parsed = parsed.unwrap();
        assert_eq!(parsed.move_no, 6);
        assert_eq!(parsed.move_1, Some(Move::Double(2)),);
        assert_eq!(parsed.move_2, Some(Move::Take));
    }

    #[test]
    fn move_quadruple_and_noplay() {
        let str = " 13) 66: 20/14 20/14 18/12* 14/8 44: ";
        let parsed = MoveTuple::parse(str, 1);
        assert!(parsed.is_ok());
        let parsed = parsed.unwrap();
        assert_eq!(parsed.move_no, 13);
        assert_eq!(
            parsed.move_1,
            Some(Move::Plays((
                DiceRoll {
                    dice_1: DiceValue::Six,
                    dice_2: DiceValue::Six
                },
                vec![
                    Play {
                        hit_status: HitStatus::NoHit,
                        from: MovePosition::Twenty,
                        to: MovePosition::Fourteen
                    },
                    Play {
                        hit_status: HitStatus::NoHit,
                        from: MovePosition::Twenty,
                        to: MovePosition::Fourteen
                    },
                    Play {
                        hit_status: HitStatus::Hit,
                        from: MovePosition::Eighteen,
                        to: MovePosition::Twelve
                    },
                    Play {
                        hit_status: HitStatus::NoHit,
                        from: MovePosition::Fourteen,
                        to: MovePosition::Eight
                    },
                ]
            )))
        );
        assert_eq!(
            parsed.move_2,
            Some(Move::CantPlay(DiceRoll {
                dice_1: DiceValue::Four,
                dice_2: DiceValue::Four
            }))
        );
    }

    #[test]
    fn first_move_parse() {
        let str = "  1)                             32: 24/21 13/11 ";
        let parsed = MoveTuple::parse(str, 1);
        assert!(parsed.is_ok());
        let parsed = parsed.unwrap();
        assert_eq!(parsed.move_no, 1);
        assert_eq!(
            parsed.move_1,
            Some(Move::Plays((
                DiceRoll {
                    dice_1: DiceValue::Three,
                    dice_2: DiceValue::Two
                },
                vec![
                    Play {
                        hit_status: HitStatus::NoHit,
                        from: MovePosition::TwentyFour,
                        to: MovePosition::TwentyOne
                    },
                    Play {
                        hit_status: HitStatus::NoHit,
                        from: MovePosition::Thirteen,
                        to: MovePosition::Eleven
                    },
                ]
            )))
        );
        assert_eq!(parsed.move_2, None,);
    }

    #[test]
    fn move_take_and_play() {
        let str = "8)  Takes                      53: 13/8 10/7";
        let parsed = MoveTuple::parse(str, 1);
        assert!(parsed.is_ok());
        let parsed = parsed.unwrap();
        assert_eq!(parsed.move_no, 8);
        assert_eq!(parsed.move_1, Some(Move::Take));
        assert_eq!(
            parsed.move_2,
            Some(Move::Plays((
                DiceRoll {
                    dice_1: DiceValue::Five,
                    dice_2: DiceValue::Three
                },
                vec![
                    Play {
                        hit_status: HitStatus::NoHit,
                        from: MovePosition::Thirteen,
                        to: MovePosition::Eight
                    },
                    Play {
                        hit_status: HitStatus::NoHit,
                        from: MovePosition::Ten,
                        to: MovePosition::Seven
                    },
                ]
            )))
        );
    }

    #[test]
    fn move_double_drop() {
        let str = "11)  Doubles => 2                Drops";
        let parsed = MoveTuple::parse(str, 1);
        assert!(parsed.is_ok());
        let parsed = parsed.unwrap();
        assert_eq!(parsed.move_no, 11);
        assert_eq!(parsed.move_1, Some(Move::Double(2)));
        assert_eq!(parsed.move_2, Some(Move::Drop));
    }

    #[test]
    fn move_then_doubles() {
        let str = " 7) 42: 25/23 25/21              Doubles => 2";
        let parsed = MoveTuple::parse(str, 1);
        assert!(parsed.is_ok());
        let parsed = parsed.unwrap();
        assert_eq!(parsed.move_no, 7);
        assert_eq!(
            parsed.move_1,
            Some(Move::Plays((
                DiceRoll {
                    dice_1: DiceValue::Four,
                    dice_2: DiceValue::Two
                },
                vec![
                    Play {
                        hit_status: HitStatus::NoHit,
                        from: MovePosition::Bar,
                        to: MovePosition::TwentyThree
                    },
                    Play {
                        hit_status: HitStatus::NoHit,
                        from: MovePosition::Bar,
                        to: MovePosition::TwentyOne
                    },
                ]
            )))
        );
        assert_eq!(parsed.move_2, Some(Move::Double(2)));
    }

    #[test]
    fn opening_legal_moves_test() {
        let mut state = GameState::new();
        state.add_move(Move::Skip);
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
        assert_eq!(state.p2_positions.get(&MovePosition::Thirteen), Some(&4));
        assert_eq!(state.p2_positions.get(&MovePosition::TwentyFour), Some(&1));
        assert_eq!(state.p2_positions.get(&MovePosition::TwentyOne), Some(&1));
        assert_eq!(state.p2_positions.get(&MovePosition::Eleven), Some(&1));
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

        let p1: Vec<String> = state
            .p1_positions
            .iter()
            .filter(|(k, v)| **v >= 1)
            .map(|(k, v)| format!("{:?}, {:?}", k.board_number(), v))
            .collect();
        let p2: Vec<String> = state
            .p2_positions
            .iter()
            .filter(|(k, v)| **v >= 1)
            .map(|(k, v)| format!("{:?}, {:?}", k.board_number(), v))
            .collect();
        let res = GameState::can_move(
            &MovePosition::Six,
            &MovePosition::Four,
            &state.p1_positions,
            &state.p2_positions,
        );
        let all_possible_moves_collected: Vec<Vec<Play>> =
            GameState::all_possible_moves(&roll, &state.p1_positions, &state.p2_positions)
                .into_iter()
                .map(|mv| mv.collect())
                .collect();
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

        let res = GameState::can_move(
            &MovePosition::Thirteen,
            &MovePosition::Eight,
            &state.p2_positions,
            &state.p1_positions,
        );
        let res2 = GameState::can_move(
            &MovePosition::Bar,
            &MovePosition::TwentyOne,
            &state.p2_positions,
            &state.p1_positions,
        );
        let all_possible_moves_collected: Vec<Vec<Play>> =
            GameState::all_possible_moves(&roll, &state.p2_positions, &state.p1_positions)
                .into_iter()
                .map(|mv| mv.collect())
                .collect();

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
