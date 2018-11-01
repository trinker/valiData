make_core <- make_core_directory <- function(path = '.', inst.name = 'CampusLabsCore', ...){


	dir.create(file.path(path, inst.name))

	invisible(lapply(file.path(path, inst.name, c('Course', 'Accounts', 'Demographics')), dir.create))
 
	subs <- file.path(path, inst.name,
		c("Course/AcademicTerm", "Course/OrgUnit", "Course/Course", 
			"Course/Section", "Course/CrossListing", "Course/Enrollment", 
			"Course/Instructor", "Course/SectionAttribute", "Course/RemoveInstructor", 
			"Course/AcademicProgram", "Accounts/AccountImports", "Demographics/FacultyImport", 
			"Demographics/StudentImport"
		)
	)

	invisible(lapply(subs, dir.create))
}

get_core_dictionary <- get_core <- function(url = 'https://campuslabs.zendesk.com/hc/en-us/article_attachments/360010472454/Core_data_dictionary.xlsx'){
	
	eval(parse(text = 'textreadr::download(url)'))

}
