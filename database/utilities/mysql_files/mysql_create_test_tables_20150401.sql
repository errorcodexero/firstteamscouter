CREATE DATABASE `firstteamscouter_test` /*!40100 DEFAULT CHARACTER SET utf8 */;

use `firstteamscouter_test`;

CREATE TABLE `competition_data` (
  `_id` bigint(20) NOT NULL AUTO_INCREMENT,
  `tablet_id` bigint(20) DEFAULT '0',
  `competition_name` varchar(255) DEFAULT 'Not Set',
  `competition_location` varchar(255) DEFAULT NULL,
  `ready_to_export` varchar(5) DEFAULT 'false',
  PRIMARY KEY (`_id`),
  UNIQUE KEY `_id_UNIQUE` (`_id`)
) ENGINE=InnoDB AUTO_INCREMENT=0 DEFAULT CHARSET=utf8;

CREATE TABLE `match_data` (
  `_id` bigint(20) NOT NULL AUTO_INCREMENT,
  `tablet_id` bigint(20) DEFAULT '0',
  `competition_id` bigint(20) DEFAULT '-1',
  `match_time` varchar(255) DEFAULT NULL,
  `match_type` varchar(255) DEFAULT 'Qualification',
  `match_number` int(11) DEFAULT NULL,
  `match_location` varchar(255) DEFAULT NULL,
  `red_team_one_id` bigint(20) DEFAULT '-1',
  `red_team_two_id` bigint(20) DEFAULT '-1',
  `red_team_three_id` bigint(20) DEFAULT '-1',
  `blue_team_one_id` bigint(20) DEFAULT '-1',
  `blue_team_two_id` bigint(20) DEFAULT '-1',
  `blue_team_three_id` bigint(20) DEFAULT '-1',
  `ready_to_export` varchar(5) DEFAULT 'false',
  PRIMARY KEY (`_id`),
  UNIQUE KEY `_id_UNIQUE` (`_id`)
) ENGINE=InnoDB AUTO_INCREMENT=0 DEFAULT CHARSET=utf8;

CREATE TABLE `notes_data` (
  `_id` bigint(20) NOT NULL DEFAULT '-1',
  `tablet_id` bigint(20) NOT NULL DEFAULT '0',
  `owner_id` bigint(20) DEFAULT '-1',
  `note_type` varchar(20) DEFAULT NULL,
  `note_text` varchar(255) DEFAULT NULL,
  `ready_to_export` varchar(5) DEFAULT 'false',
  PRIMARY KEY (`_id`,`tablet_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE TABLE `picture_data` (
  `_id` bigint(20) NOT NULL AUTO_INCREMENT,
  `tablet_id` bigint(20) NOT NULL DEFAULT '0',
  `owner_id` bigint(20) DEFAULT '-1',
  `picture_type` varchar(20) DEFAULT NULL,
  `picture_uri` varchar(255) DEFAULT NULL,
  `ready_to_export` varchar(5) DEFAULT 'false',
  PRIMARY KEY (`_id`,`tablet_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE TABLE `pit_data` (
  `_id` bigint(20) NOT NULL AUTO_INCREMENT,
  `tablet_id` bigint(20) NOT NULL DEFAULT '0',
  `pit_info` varchar(255) DEFAULT NULL,
  `ready_to_export` varchar(5) DEFAULT 'false',
  PRIMARY KEY (`_id`,`tablet_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE TABLE `robot_data` (
  `_id` bigint(20) NOT NULL DEFAULT '-1',
  `tablet_id` bigint(20) NOT NULL DEFAULT '0',
  `team_id` bigint(20) DEFAULT '-1',
  `competition_id` bigint(20) DEFAULT '-1',
  `drive_train_type` varchar(20) DEFAULT NULL,
  `wheel_type` varchar(20) DEFAULT NULL,
  `number_of_wheels` int(11) DEFAULT '0',
  `number_of_tote_stacks` int(11) DEFAULT '0',
  `number_of_totes_per_stack` int(11) DEFAULT '0',
  `number_of_cans_robot_can_handle` int(11) DEFAULT '0',
  `robot_can_get_step_cans` char(5) DEFAULT 'false',
  `robot_can_put_totes_on_step` char(5) DEFAULT 'false',
  `robot_software_language` varchar(20) DEFAULT 'C++',
  `tote_manipulator_type` varchar(20) DEFAULT 'none',
  `can_manipulator_type` varchar(20) DEFAULT 'none',
  `robot_drive_range` varchar(20) DEFAULT 'short',
  `team_does_coopertition` char(5) DEFAULT 'false',
  `robot_stacks_from` varchar(255) DEFAULT 'chute',
  `ready_to_export` varchar(5) DEFAULT 'false',
  PRIMARY KEY (`_id`,`tablet_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE TABLE `team_data` (
  `_id` bigint(20) NOT NULL AUTO_INCREMENT,
  `tablet_id` bigint(20) DEFAULT '0',
  `team_number` int(11) DEFAULT NULL,
  `team_sub_number` int(11) DEFAULT NULL,
  `team_name` varchar(255) DEFAULT NULL,
  `team_city` varchar(255) DEFAULT NULL,
  `team_state` varchar(255) DEFAULT NULL,
  `num_team_members` int(11) DEFAULT NULL,
  `team_creation_year` int(11) DEFAULT NULL,
  `ready_to_export` varchar(5) DEFAULT 'false',
  PRIMARY KEY (`_id`),
  UNIQUE KEY `_id_UNIQUE` (`_id`)
) ENGINE=InnoDB AUTO_INCREMENT=0 DEFAULT CHARSET=utf8;

CREATE TABLE `team_match` (
  `_id` bigint(20) NOT NULL AUTO_INCREMENT,
  `tablet_id` bigint(20) NOT NULL DEFAULT '0',
  `team_id` bigint(20) DEFAULT '-1',
  `match_id` bigint(20) DEFAULT '-1',
  `competition_id` bigint(20) DEFAULT '-1',
  `alliance_position` varchar(20) DEFAULT 'Not Set',
  `broke_down` char(5) DEFAULT 'false',
  `no_move` char(5) DEFAULT 'false',
  `lost_connection` char(5) DEFAULT 'false',
  `starting_location` int(11) DEFAULT '0',
  `starting_location_X` int(11) DEFAULT '0',
  `starting_location_Y` int(11) DEFAULT '0',
  `starting_location_on_field` varchar(255) DEFAULT 'none',
  `auto_totes_picked_up` int(11) DEFAULT '0',
  `auto_totes_stacked` int(11) DEFAULT '0',
  `auto_totes_scored` int(11) DEFAULT '0',
  `auto_cans_picked_up` int(11) DEFAULT '0',
  `auto_cans_scored` int(11) DEFAULT '0',
  `auto_cans_pulled_from_step` int(11) DEFAULT '0',
  `auto_mode_saved` varchar(5) DEFAULT 'false',
  `auto_final_location_X` int(11) DEFAULT '0',
  `auto_final_location_Y` int(11) DEFAULT '0',
  `auto_tote_1_location_X` int(11) DEFAULT '0',
  `auto_tote_1_location_Y` int(11) DEFAULT '0',
  `auto_tote_2_location_X` int(11) DEFAULT '0',
  `auto_tote_2_location_Y` int(11) DEFAULT '0',
  `auto_tote_3_location_X` int(11) DEFAULT '0',
  `auto_tote_3_location_Y` int(11) DEFAULT '0',
  `auto_can_1_location_X` int(11) DEFAULT '0',
  `auto_can_1_location_Y` int(11) DEFAULT '0',
  `auto_can_2_location_X` int(11) DEFAULT '0',
  `auto_can_2_location_Y` int(11) DEFAULT '0',
  `auto_can_3_location_X` int(11) DEFAULT '0',
  `auto_can_3_location_Y` int(11) DEFAULT '0',
  `auto_can_4_location_X` int(11) DEFAULT '0',
  `auto_can_4_location_Y` int(11) DEFAULT '0',
  `auto_can_5_location_X` int(11) DEFAULT '0',
  `auto_can_5_location_Y` int(11) DEFAULT '0',
  `auto_can_6_location_X` int(11) DEFAULT '0',
  `auto_can_6_location_Y` int(11) DEFAULT '0',
  `auto_can_7_location_X` int(11) DEFAULT '0',
  `auto_can_7_location_Y` int(11) DEFAULT '0',
  `auto_robot_visible` varchar(5) DEFAULT 'false',
  `auto_tote1_visible` varchar(5) DEFAULT 'false',
  `auto_tote2_visible` varchar(5) DEFAULT 'false',
  `auto_tote3_visible` varchar(5) DEFAULT 'false',
  `auto_can1_visible` varchar(5) DEFAULT 'false',
  `auto_can2_visible` varchar(5) DEFAULT 'false',
  `auto_can3_visible` varchar(5) DEFAULT 'false',
  `auto_can4_visible` varchar(5) DEFAULT 'false',
  `auto_can5_visible` varchar(5) DEFAULT 'false',
  `auto_can6_visible` varchar(5) DEFAULT 'false',
  `auto_can7_visible` varchar(5) DEFAULT 'false',
  `auto_robot_stack_list` varchar(255) DEFAULT 'none',
  `team_match_notes` varchar(255) DEFAULT 'none',
  `tote_stacker` varchar(5) DEFAULT 'false',
  `can_kinger` varchar(5) DEFAULT 'false',
  `cooperative` varchar(5) DEFAULT 'false',
  `noodler` varchar(5) DEFAULT 'false',
  `ni` varchar(5) DEFAULT 'false',
  `tote_control_inside_robot` varchar(5) DEFAULT 'false',
  `tote_control_fork_lift` varchar(5) DEFAULT 'false',
  `tote_control_handle_grabber` varchar(5) DEFAULT 'false',
  `tote_control_drop_alot` varchar(5) DEFAULT 'false',
  `tote_control_great_control` varchar(5) DEFAULT 'false',
  `ready_to_export` varchar(5) DEFAULT 'false',
  PRIMARY KEY (`_id`,`tablet_id`),
  UNIQUE KEY `_id_UNIQUE` (`_id`)
) ENGINE=InnoDB AUTO_INCREMENT=0 DEFAULT CHARSET=utf8;

CREATE TABLE `team_match_transaction` (
  `_id` bigint(20) NOT NULL AUTO_INCREMENT,
  `tablet_id` bigint(20) NOT NULL DEFAULT '0',
  `team_id` bigint(20) DEFAULT '-1',
  `match_id` bigint(20) DEFAULT '-1',
  `transaction_timestamp` varchar(16) DEFAULT '-1',
  `action_name` varchar(20) DEFAULT 'None',
  `action_phase` varchar(20) DEFAULT 'None',
  `action_start_location_name` varchar(20) DEFAULT 'none',
  `action_start_location_X` int(11) DEFAULT 0,
  `action_start_location_Y` int(11) DEFAULT 0,
  `action_end_location_name` varchar(20) DEFAULT 'none',
  `action_end_location_X` int(11) DEFAULT 0,
  `action_end_location_Y` int(11) DEFAULT 0,
  `element_types` varchar(255) DEFAULT 'none',
  `element_states` varchar(255) DEFAULT 'none',
  `ready_to_export` varchar(5) DEFAULT 'false',
  PRIMARY KEY (`_id`,`tablet_id`)
) ENGINE=InnoDB AUTO_INCREMENT=0 DEFAULT CHARSET=utf8;
