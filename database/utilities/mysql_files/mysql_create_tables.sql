CREATE TABLE IF NOT EXISTS season_data (
_id integer primary key auto_increment,
season_name varchar(255),
season_year integer,
ready_to_export varchar(5));

CREATE TABLE IF NOT EXISTS competition_data (
_id integer primary key auto_increment,
tablet_id integer,
competition_name varchar(255),
competition_location varchar(255),
ready_to_export varchar(5));


CREATE TABLE IF NOT EXISTS match_data (
_id integer primary key auto_increment,
tablet_id integer,
competition_id integer,
match_time varchar(255),
match_type varchar(255),
match_number integer,
match_location varchar(255),
red_team_one_id integer,
red_team_two_id integer,
red_team_three_id integer,
blue_team_one_id integer,
blue_team_two_id integer,
blue_team_three_id integer,
ready_to_export varchar(5));


CREATE TABLE IF NOT EXISTS notes_data (
_id integer primary key auto_increment,
tablet_id integer,
owner_id integer,
note_type varchar(20),
note_text varchar(255),
ready_to_export varchar(5));


CREATE TABLE IF NOT EXISTS picture_data (
_id integer primary key auto_increment,
tablet_id integer,
owner_id integer,
picture_type varchar(20),
picture_uri varchar(255),
ready_to_export varchar(5));


CREATE TABLE IF NOT EXISTS pit_data (
_id integer primary key auto_increment,
tablet_id integer,
pit_info varchar(255),
ready_to_export varchar(5));


CREATE TABLE IF NOT EXISTS robot_data (
_id integer primary key auto_increment,
tablet_id integer,
team_id integer,
competition_id integer,
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
robot_stacks_from varchar(255),
ready_to_export varchar(5));


CREATE TABLE IF NOT EXISTS team_data (
_id integer primary key auto_increment,
tablet_id integer,
team_number integer,
team_sub_number integer,
team_name varchar(255),
team_city varchar(255),
team_state varchar(255),
num_team_members integer,
team_creation_year integer,
ready_to_export varchar(5));


CREATE TABLE IF NOT EXISTS team_match (
_id integer auto_increment,
tablet_id integer,
team_id integer,
match_id integer,
competition_id integer,
alliance_position varchar(20),
broke_down char(5),
no_move char(5),
lost_connection char(5),
starting_location integer,
starting_location_X integer,
starting_location_Y integer,
starting_location_on_field varchar(255),
auto_totes_picked_up integer,
auto_totes_stacked integer,
auto_totes_scored integer,
auto_cans_picked_up integer,
auto_cans_scored integer,
auto_cans_pulled_from_step integer,
auto_mode_saved varchar(5),
auto_final_location_X integer,
auto_final_location_Y integer,
auto_tote_1_location_X integer,
auto_tote_1_location_Y integer,
auto_tote_2_location_X integer,
auto_tote_2_location_Y integer,
auto_tote_3_location_X integer,
auto_tote_3_location_Y integer,
auto_can_1_location_X integer,
auto_can_1_location_Y integer,
auto_can_2_location_X integer,
auto_can_2_location_Y integer,
auto_can_3_location_X integer,
auto_can_3_location_Y integer,
auto_can_4_location_X integer,
auto_can_4_location_Y integer,
auto_can_5_location_X integer,
auto_can_5_location_Y integer,
auto_can_6_location_X integer,
auto_can_6_location_Y integer,
auto_can_7_location_X integer,
auto_can_7_location_Y integer,
auto_robot_visible varchar(5),
auto_tote1_visible varchar(5),
auto_tote2_visible varchar(5),
auto_tote3_visible varchar(5),
auto_can1_visible varchar(5),
auto_can2_visible varchar(5),
auto_can3_visible varchar(5),
auto_can4_visible varchar(5),
auto_can5_visible varchar(5),
auto_can6_visible varchar(5),
auto_can7_visible varchar(5),
auto_robot_stack_list varchar(255),
team_match_notes varchar(255),
tote_stacker varchar(5),
can_kinger varchar(5),
cooperative varchar(5),
noodler varchar(5),
ni varchar(5),
tote_control_inside_robot varchar(5),
tote_control_fork_lift varchar(5),
tote_control_handle_grabber varchar(5),
tote_control_drop_alot varchar(5),
tote_control_great_control varchar(5),
ready_to_export varchar(5),
primary key(_id, tablet_id));


CREATE TABLE IF NOT EXISTS team_match_transaction (
_id integer auto_increment,
tablet_id integer,
team_id integer,
match_id integer,
transaction_timestamp varchar(16),
action_name varchar(20),
action_phase varchar(20),
action_start_location_name varchar(20),
action_start_location_X integer,
action_start_location_Y integer,
action_end_location_name varchar(20),
action_end_location_X integer,
action_end_location_Y integer,
element_types varchar(255),
element_states varchar(255),
ready_to_export varchar(5),
primary key(_id, tablet_id));
