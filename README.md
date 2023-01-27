# WEAVER: The Dendritic Chat Platform

WEAVER is an R Shiny application for dendritic chatting. 

## What is Dendritic Chat?

Dendritic chat is a framework made to structure multifarious conversations between two or more individuals. 
As users respond, chat bubbles are formed, where users can choose to respond to specific bubbles. The
application forms a graph of the conversation. 

![alt text](https://github.com/ap0phasi/weaverchat/blob/main/Figures/WeaverOverview.png?raw=true)

Users can click chat bubbles or manually set the At value to respond to that message. Messages that have 
not been addressed begin to wilt, with the color fading until they are white. 

## Functionality

In addition to sending messages, users can set collapses and trunks. 

A collapse on a message removes that chat bubble and all downstream messages from the user's view. 

![alt text](https://github.com/ap0phasi/weaverchat/blob/main/Figures/WeaverCollapse.png?raw=true)

A trunk on a message removes all chat bubbles except for the trunked message and its downstream messages. 

![alt text](https://github.com/ap0phasi/weaverchat/blob/main/Figures/WeaverTrunk.png?raw=true)

Multiple trunks and collapses can be set with comma-separated values in the respective text boxes. 

![alt text](https://github.com/ap0phasi/weaverchat/blob/main/Figures/WeaverMulti.png?raw=true)

If a user wishes to start a new tree without responding to a specific message they can simply write
a message with At set to 0. 
