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

#Slice: {
	key:    string
	title:  string
	goal:   string
	status: #Status
	note:   string
	repos?:       [...string]
	depends_on?:  [...string]
	steps:        [...#Step]
	final_steps:  [...#Step]
}
