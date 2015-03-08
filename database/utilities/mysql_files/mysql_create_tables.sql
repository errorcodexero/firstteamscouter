CREATE TABLE IF NOT EXISTS competition_data (
_id integer primary key auto_increment,
competition_name varchar(20),
competition_location varchar(20));


CREATE TABLE IF NOT EXISTS competition_matches (
_id integer primary key auto_increment,
competition_id integer,
match_id integer);


CREATE TABLE IF NOT EXISTS match_data (
_id integer primary key auto_increment,
competition_id integer,
match_time varchar(20),
match_type varchar(20),
match_number integer,
match_location varchar(20),
red_team_one_id integer,
red_team_two_id integer,
red_team_three_id integer,
blue_team_one_id integer,
blue_team_two_id integer,
blue_team_three_id integer);


CREATE TABLE IF NOT EXISTS match_notes (
_id integer primary key auto_increment,
match_id integer,
note_id integer);


CREATE TABLE IF NOT EXISTS notes_data (
_id integer primary key auto_increment,
note_type varchar(20),
note_text varchar(255));


CREATE TABLE IF NOT EXISTS picture_data (
_id integer primary key auto_increment,
picture_type varchar(20),
picture_uri varchar(255));


CREATE TABLE IF NOT EXISTS pit_data (
_id integer primary key auto_increment,
pit_info varchar(20));


CREATE TABLE IF NOT EXISTS pit_notes (
_id integer primary key auto_increment,
pit_id integer,
note_id integer);


CREATE TABLE IF NOT EXISTS pit_pictures (
_id integer primary key auto_increment,
pit_id integer,
picture_id integer);


CREATE TABLE IF NOT EXISTS robot_data (
_id integer primary key auto_increment,
drive_train_type varchar(20),
wheel_type varchar(20),
number_of_wheels integer,
number_of_tote_stacks integer,
number_of_totes_per_stack integer,
number_of_cans_robot_can_handle integer,
robot_can_get_step_cans char(5),
robot_can_put_totes_on_step char(5),
robot_software_language varchar(20),
tote_manipulator_type varchar(20),
can_manipulator_type varchar(20),
robot_drive_range varchar(20),
team_does_coopertition char(5),
robot_stacks_from varchar(20));


CREATE TABLE IF NOT EXISTS robot_notes (
_id integer primary key auto_increment,
robot_id integer,
note_id integer);


CREATE TABLE IF NOT EXISTS robot_pictures (
_id integer primary key auto_increment,
robot_id integer,
picture_id integer);


CREATE TABLE IF NOT EXISTS team_data (
_id integer,
team_number integer,
team_sub_number integer,
team_name varchar(20),
team_location varchar(20),
num_team_members integer,
team_creation_year integer,
primary key ( team_number, team_sub_number ));


CREATE TABLE IF NOT EXISTS team_match (
_id integer primary key auto_increment,
team_id integer,
match_id integer,
alliance_position varchar(20),
broke_down char(5),
no_move char(5),
lost_connection char(5),
starting_location integer,
starting_location_X integer,
starting_location_Y integer,
starting_location_on_field varchar(20),
auto_totes_picked_up integer,
auto_totes_stacked integer,
auto_totes_scored integer,
auto_cans_picked_up integer,
auto_cans_scored integer,
auto_cans_pulled_from_step integer,
auto_final_location_X integer,
auto_final_location_Y integer,
team_match_notes varchar(255));


CREATE TABLE IF NOT EXISTS team_match_notes (
_id integer primary key auto_increment,
team_id integer,
match_id integer,
note_id integer);


CREATE TABLE IF NOT EXISTS team_pits (
_id integer primary key auto_increment,
team_id integer,
pit_id integer);



CREATE TABLE IF NOT EXISTS team_robots (
_id integer primary key auto_increment,
team_id integer,
robot_id integer);


CREATE TABLE IF NOT EXISTS team_match_transaction (
_id integer primary key auto_increment,
team_id integer,
match_id integer,
timestamp varchar(16),
action_name varchar(20),
action_phase varchar(20),
action_start_location_name varchar(20),
action_start_location_X integer,
action_start_location_Y integer,
action_end_location_name varchar(20),
action_end_location_X integer,
action_end_location_Y integer,
element_types varchar(255),
element_states varchar(255));


CREATE TABLE IF NOT EXISTS team_match_transactions (
_id integer primary key auto_increment,
team_match_id integer,
transaction_id integer);

