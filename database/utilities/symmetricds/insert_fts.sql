--
-- Licensed to JumpMind Inc under one or more contributor
-- license agreements.  See the NOTICE file distributed
-- with this work for additional information regarding
-- copyright ownership.  JumpMind Inc licenses this file
-- to you under the GNU General Public License, version 3.0 (GPLv3)
-- (the "License"); you may not use this file except in compliance
-- with the License.
--
-- You should have received a copy of the GNU General Public License,
-- version 3.0 (GPLv3) along with this library; if not, see
-- <http://www.gnu.org/licenses/>.
--
-- Unless required by applicable law or agreed to in writing,
-- software distributed under the License is distributed on an
-- "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
-- KIND, either express or implied.  See the License for the
-- specific language governing permissions and limitations
-- under the License.
--

------------------------------------------------------------------------------
-- Sample Data
------------------------------------------------------------------------------
--insert into item (item_id, name) values (11000001, 'Yummy Gum');
--insert into item_selling_price (item_id, store_id, price, cost) values (11000001, '001',0.20, 0.10);
--insert into item_selling_price (item_id, store_id, price, cost) values (11000001, '002',0.30, 0.20);

--insert into sale_transaction (tran_id, store_id, workstation, day, seq) 
--values (900, '001', '3', '2012-12-01', 90);
--insert into sale_return_line_item (tran_id, item_id, price, quantity, returned_quantity)
--values (900, 11000001, 0.20, 1, 0);

------------------------------------------------------------------------------
-- Sample Symmetric Configuration
------------------------------------------------------------------------------

delete from sym_trigger_router;
delete from sym_trigger;
delete from sym_router;
delete from sym_channel where channel_id in ('master_data', 'node_data');
delete from sym_node_group_link;
delete from sym_node_group;
delete from sym_node_host;
delete from sym_node_identity;
delete from sym_node_security;
delete from sym_node;


-- CHANNELS --
insert into sym_channel 
(channel_id, processing_order, max_batch_size, enabled, description)
values('master_data', 1, 100000, 1, 'All data to fts nodes.');

insert into sym_channel
(channel_id, processing_order, max_batch_size, enabled, description)
values('node_data', 1, 100000, 1, 'All data from fts nodes.');

--insert into sym_channel
--(channel_id, processing_order, max_batch_size, enabled, description)
--values('item', 1, 100000, 1, 'Item and pricing data');




-- GROUPS --
insert into sym_node_group (node_group_id) values ('fts-master');
insert into sym_node_group (node_group_id) values ('fts-node');


-- GROUP LINKS --
insert into sym_node_group_link (source_node_group_id, target_node_group_id, data_event_action) values ('fts-master', 'fts-node', 'W');
insert into sym_node_group_link (source_node_group_id, target_node_group_id, data_event_action) values ('fts-node', 'fts-master', 'P');



-- ROUTERS --
insert into sym_router 
(router_id,source_node_group_id,target_node_group_id,router_type,create_time,last_update_time)
values('fts_master_2_nodes', 'fts-master', 'fts-node', 'default',current_timestamp, current_timestamp);

insert into sym_router 
(router_id,source_node_group_id,target_node_group_id,router_type,create_time,last_update_time)
values('fts_node_2_master', 'fts-node', 'fts-master', 'default',current_timestamp, current_timestamp);

--insert into sym_router 
--(router_id,source_node_group_id,target_node_group_id,router_type,router_expression,create_time,last_update_time)
--values('fts_master_2_one_node', 'fts-master', 'fts-node', 'column','STORE_ID=:EXTERNAL_ID or OLD_STORE_ID=:EXTERNAL_ID',current_timestamp, current_timestamp);





-- TRIGGERS --
insert into sym_trigger 
(trigger_id,source_table_name,channel_id,last_update_time,create_time)
values('all_data_2_nodes','*','master_data',current_timestamp,current_timestamp);

insert into sym_trigger
(trigger_id,source_table_name,channel_id,last_update_time,create_time)
values('all_data_2_master','*','node_data',current_timestamp,current_timestamp);






-- TRIGGER ROUTERS --
insert into sym_trigger_router 
(trigger_id,router_id,initial_load_order,last_update_time,create_time)
values('all_data_2_nodes','fts_master_2_nodes', 100, current_timestamp, current_timestamp);

insert into sym_trigger_router 
(trigger_id,router_id,initial_load_order,last_update_time,create_time)
values('all_data_2_master','fts_node_2_master', 200, current_timestamp, current_timestamp);



-- NODES --
insert into sym_node (node_id,node_group_id,external_id,sync_enabled,sync_url,schema_version,symmetric_version,database_type,database_version,heartbeat_time,timezone_offset,batch_to_send_count,batch_in_error_count,created_at_node_id) 
 values ('000','fts-master','master',1,null,null,null,null,null,current_timestamp,null,0,0,'000');
insert into sym_node (node_id,node_group_id,external_id,sync_enabled,sync_url,schema_version,symmetric_version,database_type,database_version,heartbeat_time,timezone_offset,batch_to_send_count,batch_in_error_count,created_at_node_id) 
 values ('001','fts-node','Scout1',1,null,null,null,null,null,current_timestamp,null,0,0,'000');
insert into sym_node (node_id,node_group_id,external_id,sync_enabled,sync_url,schema_version,symmetric_version,database_type,database_version,heartbeat_time,timezone_offset,batch_to_send_count,batch_in_error_count,created_at_node_id) 
 values ('002','fts-node','Scout2',1,null,null,null,null,null,current_timestamp,null,0,0,'000');
 insert into sym_node (node_id,node_group_id,external_id,sync_enabled,sync_url,schema_version,symmetric_version,database_type,database_version,heartbeat_time,timezone_offset,batch_to_send_count,batch_in_error_count,created_at_node_id) 
 values ('003','fts-node','Scout3',1,null,null,null,null,null,current_timestamp,null,0,0,'000');
 insert into sym_node (node_id,node_group_id,external_id,sync_enabled,sync_url,schema_version,symmetric_version,database_type,database_version,heartbeat_time,timezone_offset,batch_to_send_count,batch_in_error_count,created_at_node_id) 
 values ('004','fts-node','Scout4',1,null,null,null,null,null,current_timestamp,null,0,0,'000');
 insert into sym_node (node_id,node_group_id,external_id,sync_enabled,sync_url,schema_version,symmetric_version,database_type,database_version,heartbeat_time,timezone_offset,batch_to_send_count,batch_in_error_count,created_at_node_id) 
 values ('005','fts-node','Scout5',1,null,null,null,null,null,current_timestamp,null,0,0,'000');
 insert into sym_node (node_id,node_group_id,external_id,sync_enabled,sync_url,schema_version,symmetric_version,database_type,database_version,heartbeat_time,timezone_offset,batch_to_send_count,batch_in_error_count,created_at_node_id) 
 values ('006','fts-node','Scout6',1,null,null,null,null,null,current_timestamp,null,0,0,'000');
 insert into sym_node (node_id,node_group_id,external_id,sync_enabled,sync_url,schema_version,symmetric_version,database_type,database_version,heartbeat_time,timezone_offset,batch_to_send_count,batch_in_error_count,created_at_node_id) 
 values ('007','fts-node','Scout7',1,null,null,null,null,null,current_timestamp,null,0,0,'000');
 insert into sym_node (node_id,node_group_id,external_id,sync_enabled,sync_url,schema_version,symmetric_version,database_type,database_version,heartbeat_time,timezone_offset,batch_to_send_count,batch_in_error_count,created_at_node_id) 
 values ('008','fts-node','Scout8',1,null,null,null,null,null,current_timestamp,null,0,0,'000');
 insert into sym_node (node_id,node_group_id,external_id,sync_enabled,sync_url,schema_version,symmetric_version,database_type,database_version,heartbeat_time,timezone_offset,batch_to_send_count,batch_in_error_count,created_at_node_id) 
 values ('009','fts-node','Scout9',1,null,null,null,null,null,current_timestamp,null,0,0,'000');
 insert into sym_node (node_id,node_group_id,external_id,sync_enabled,sync_url,schema_version,symmetric_version,database_type,database_version,heartbeat_time,timezone_offset,batch_to_send_count,batch_in_error_count,created_at_node_id) 
 values ('010','fts-node','Scout10',1,null,null,null,null,null,current_timestamp,null,0,0,'000');


 
-- PASSWORDS --
insert into sym_node_security (node_id,node_password,registration_enabled,registration_time,initial_load_enabled,initial_load_time,created_at_node_id) 
 values ('000','5d1c92bbacbe2edb9e1ca5dbb0e481',0,current_timestamp,0,current_timestamp,'000');
insert into sym_node_security (node_id,node_password,registration_enabled,registration_time,initial_load_enabled,initial_load_time,created_at_node_id) 
 values ('001','5d1c92bbacbe2edb9e1ca5dbb0e481',1,null,1,null,'000');
insert into sym_node_security (node_id,node_password,registration_enabled,registration_time,initial_load_enabled,initial_load_time,created_at_node_id) 
 values ('002','5d1c92bbacbe2edb9e1ca5dbb0e481',1,null,1,null,'000');
 insert into sym_node_security (node_id,node_password,registration_enabled,registration_time,initial_load_enabled,initial_load_time,created_at_node_id) 
 values ('003','5d1c92bbacbe2edb9e1ca5dbb0e481',1,null,1,null,'000');
 insert into sym_node_security (node_id,node_password,registration_enabled,registration_time,initial_load_enabled,initial_load_time,created_at_node_id) 
 values ('004','5d1c92bbacbe2edb9e1ca5dbb0e481',1,null,1,null,'000');
 insert into sym_node_security (node_id,node_password,registration_enabled,registration_time,initial_load_enabled,initial_load_time,created_at_node_id) 
 values ('005','5d1c92bbacbe2edb9e1ca5dbb0e481',1,null,1,null,'000');
 insert into sym_node_security (node_id,node_password,registration_enabled,registration_time,initial_load_enabled,initial_load_time,created_at_node_id) 
 values ('006','5d1c92bbacbe2edb9e1ca5dbb0e481',1,null,1,null,'000');
 insert into sym_node_security (node_id,node_password,registration_enabled,registration_time,initial_load_enabled,initial_load_time,created_at_node_id) 
 values ('007','5d1c92bbacbe2edb9e1ca5dbb0e481',1,null,1,null,'000');
 insert into sym_node_security (node_id,node_password,registration_enabled,registration_time,initial_load_enabled,initial_load_time,created_at_node_id) 
 values ('008','5d1c92bbacbe2edb9e1ca5dbb0e481',1,null,1,null,'000');
 insert into sym_node_security (node_id,node_password,registration_enabled,registration_time,initial_load_enabled,initial_load_time,created_at_node_id) 
 values ('009','5d1c92bbacbe2edb9e1ca5dbb0e481',1,null,1,null,'000');
 insert into sym_node_security (node_id,node_password,registration_enabled,registration_time,initial_load_enabled,initial_load_time,created_at_node_id) 
 values ('010','5d1c92bbacbe2edb9e1ca5dbb0e481',1,null,1,null,'000');

 
 
 -- IDENTITIES --
insert into sym_node_identity values ('000');
