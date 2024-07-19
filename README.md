# Extracting Actions from Keystroke Data

Scripts from the manuscript titled "Unveiling the Dynamics of Source-Based Writing: Insights from Keystroke Logging and Process Mining" (under review)
by Fien De Smedt, Rianne Conijn, Nina Vendermeulen, Karen Putzeys, Bram De Wever, and Hilde Van Keer.

The code details the following steps as described in the paper:
(1) Extraction and cleaning of keystroke data -- load_data.R & transform_times.R

(2) Extraction of revision events as needed for (3) -- add_revision_info.R (following https://github.com/RConijn/RevisionEvent)

(3) Extraction of actions needed for the process mining analysis -- create_indicators.R


Please note that the code is written for data extracted from Inputlog (in particular the General Analysis file), but may be adapted to other sources of data as well.
In particular, the following actions within the keystrokes are distinguished:
- Initial reading assignment
- Rereading assignment
- Initial reading sources
- Rereading sources
- Off task
- Shift to writing document
- Immediate large deletion
- Immediate small deletion
- Distant large deletion
- Distant small deletion
- Distant large insertion
- Distant small insertion
- Local navigation
- Global navigation
- Generation large chunk of text
- Generation small chunk of text
- Unknown
