make_core_directory <- function(path = '.', inst.name = 'CampusLabsCore', ...){


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
