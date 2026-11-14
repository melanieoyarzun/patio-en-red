# Codebook

All identifiers are study codes created for this release. The files contain no names or links to administrative records. Class codes follow the order of the SI tables, so `class_id = 16` is "Class ID 16" in Figure 2.

See the [data README](README.md) for the input and output files and the commands that generate them.

## `students.csv` — one row per child (859 rows)

| Variable | Description |
|---|---|
| `student_id` | Study code of the child |
| `class_id` | Classroom (1–45) |
| `school_id` | School (1–14) |
| `grade` | Grade level (3, 4 or 5, Chilean primary school) |
| `female` | 1 = girl, 0 = boy |
| `age` | Age in years on the day of the experiment (one decimal) |
| `gpa` | Grade point average, Chilean 1–7 scale |
| `attendance` | Attendance in the previous semester (% of school days); used in the balance tests |
| `attendance_2017` | Attendance in the 2017 school year (%); used in SI Table S1 |
| `guardian_secondary` | 1 = guardian completed secondary education, or has any technical or higher education |
| `guardian_education` | Guardian education in four levels (primary incomplete or none; primary complete; secondary incomplete; secondary complete or more) |
| `repeated_grade` | 1 = the child repeated a grade, 0 = never; missing when not recorded (249 children) |

## `dyads.csv` — one row per directed decision (17,804 rows)

| Variable | Description |
|---|---|
| `class_id` | Classroom |
| `round` | Classroom round in which the pair played |
| `sender_id` | Child who decides |
| `receiver_id` | Partner who receives |
| `tokens_sent` | Tokens (0–10) the sender gave to the receiver. Tokens given were doubled for the receiver |
| `seconds_since_start` | Seconds between the start of the class session and this decision. Missing in 16 classrooms where the tablets did not record the time of each decision |
| `nom_q1` … `nom_q9` | 1 if the sender named the receiver in nomination question *k* |

Nomination questions (each child named 2 to 5 classmates):

1. would most like to hang out with
2. would least like to hang out with
3. **best friends** (defines friendship type)
4. **most popular and well-known** (Social Rank, positive)
5. **not popular or well-known** (Social Rank, negative)
6. start fights
7. make fun of others
8. are kind to others
9. cooperate

## `figure2_layout.csv` — node positions for the assembled Figure 2 (9 rows)

| Variable | Description |
|---|---|
| `student_id` | Study code of a child in class 16 |
| `x`, `y` | Position of the child in the paper's drawing of Figure 2 (scaled to −1…1) |

These coordinates reproduce the drawing in Figure 2 and are used only for plotting.

## Generated measures

`analysis_students.csv` adds three measures to the student input.

| Variable | Description |
|---|---|
| `pagerank` | Weighted PageRank on the popularity nomination network within each classroom, using damping 0.85 |
| `social_rank` | PageRank rescaled to 0–1 within each classroom |
| `attendance_z` | Standardized prior-semester attendance |

`analysis_dyads.csv` adds measures based on paired decisions, friendship nominations, and the sender's allocation history.

| Variable | Description |
|---|---|
| `tokens_received` | Tokens sent by the partner to the focal child, on the original 0–10 allocation scale |
| `coop_gap` | Tokens received minus tokens sent |
| `friendship` | Mutual, sender declares, receiver declares, or non-friends, based on best-friend nominations |
| `sender_rank`, `receiver_rank` | Social Rank of each child |
| `rank_gap` | Sender's Social Rank minus the receiver's |
| `departure` | Absolute distance between a transfer and the sender's mean transfer, in units of the sender's population standard deviation. A sender with constant transfers has a departure of zero. |
| `tokens_sent_filtered`, `tokens_received_filtered` | The corresponding transfers retained by the null-model filter. Excluded transfers are missing. |
| `coop_gap_filtered` | Cooperation GAP when both transfers pass the null-model filter |

`alternative_ranks.csv` contains the original `social_rank` and the measures used for the comparisons in SI sections S4 and S8.

| Variable | Description |
|---|---|
| `positive_in`, `negative_in` | Counts of positive and negative popularity nominations received |
| `pr_positive`, `pr_negative` | PageRank on the positive and negative popularity networks, respectively |
| `pn_in` | Incoming PN centrality on the signed popularity network |
| `rank_positive_only`, `rank_pn_centrality` | Positive PageRank and incoming PN centrality, respectively, rescaled within classroom |
| `rank_two_layer` | Standardized positive PageRank minus standardized negative PageRank, rescaled within classroom |
| `rank_net_nominations` | Standardized positive indegree minus standardized negative indegree, rescaled within classroom |
| `rank_pr_minus_neg_in` | Standardized positive PageRank minus standardized negative indegree, rescaled within classroom |

`behavioral_rank.csv` contains `bsr_pagerank`, the PageRank on the classroom network weighted by token transfers, and `bsr`, that score rescaled to 0–1 within each classroom.
