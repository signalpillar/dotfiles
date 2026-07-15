package task

#Status: "planned" | "in_progress" | "blocked" | "done" | "skipped"

#Step: {
	key:    string
	title:  string
	status: #Status
	note:   string
}

#Decision: {
	date:   string
	note:   string
	source: string
}

#Artifact: {
	status: #Status
	path?:  string
	note:   string
}

#PRStatus: "open" | "merged" | "closed"

#PR: {
	url:    string
	status: #PRStatus
	note:   string
}

#RepoWork: {
	worktree?: string
	prs?:      [...#PR]
}

#Slice: {
	key:    string
	title:  string
	goal:   string
	status: #Status
	note:   string
	repos?:       [...string]
	depends_on?:  [...string]
	repo_work?: {[string]: #RepoWork}
	steps:        [...#Step]
	final_steps:  [...#Step]
}
