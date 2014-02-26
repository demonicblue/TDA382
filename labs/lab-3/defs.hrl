% This record defines the structure of the 
% client process. 
% 
% It contains the following fields: 
%
% gui: it stores the name (or Pid) of the GUI process.
%
-record(cl_st, {gui, nick, server, channels}).
    
% This record defines the structure of the 
% server process. 
% 
-record(server_st, {name, clients, nick_to_channel, channels}).

% Channel record
-record(channel_st, {name, clients}).
