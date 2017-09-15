Feature: POSIX privileges acceptance mixed tests

  Background:
    Given environment is up
    And user1 starts oneclient in /home/user1/onedata using token
    And user opened browser window
    And user of browser opened Onezone URL
    And user of browser clicked on the "plgrid" login button
    And user of browser clicked on the "user1" link
    And user of browser expanded the "go to your files" Onezone sidebar panel
    And user of browser clicked on the "p1" provider in Onezone providers sidebar panel
    And user of browser clicked on the "Go to your files" button in provider popup
    And user of browser seen that Oneprovider session has started


  Scenario: User changes file permission using oneclient and sees in browser that it has changed

    # Create file                
    When user1 creates regular files [space1/file1]

    # Change permission code
    And user1 changes space1/file1 mode to 775

    # Check permission code
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser selects "file1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Change element permissions"
    And user of browser sees that "Edit permissions" modal has appeared
    And user of browser selects "POSIX" permission type in active modal
    Then user of browser sees that current permission is "775"

    # Clean up
    And user1 deletes files [space1/file1]


  Scenario: User changes dir permission using oneclient and sees in browser that it has changed

    # Create directory                
    When user1 creates directories [space1/dir1]

    # Change permission code
    And user1 changes space1/dir1 mode to 664

    # Check permission code
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser selects "dir1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Change element permissions"
    And user of browser sees that "Edit permissions" modal has appeared
    And user of browser selects "POSIX" permission type in active modal
    Then user of browser sees that current permission is "664"

    # Clean up
    And user1 deletes empty directories [space1/dir1]


  Scenario: User changes file permission using web gui and using oneclient he sees that it has changed

    # Create file
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    
	# Change permission code
    And user of browser selects "file1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Change element permissions"
    And user of browser sees that "Edit permissions" modal has appeared
    And user of browser selects "POSIX" permission type in active modal
    And user of browser clicks on input box in active modal
    And user of browser sets "775" permission code in active modal
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared  

    #Check permission code
    Then mode of user1's space1/file1 is 775

    # Clean up
    And user1 deletes files [space1/file1]


  Scenario: User changes directory permission using web gui and using oneclient he sees that it has changed

    # Create directory
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    
	# Change permission code
    And user of browser selects "dir1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Change element permissions"
    And user of browser sees that "Edit permissions" modal has appeared
    And user of browser selects "POSIX" permission type in active modal
    And user of browser clicks on input box in active modal
    And user of browser sets "664" permission code in active modal
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared  

    #Check permission code
    Then mode of user1's space1/dir1 is 664

    # Clean up
    And user1 deletes empty directories [space1/dir1]


  Scenario: User changes file permission using web gui and using oneclient he sees that status-change time has changed
    
    # Create file
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    
	# Change permission code
    And user of browser selects "file1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Change element permissions"
    And user of browser sees that "Edit permissions" modal has appeared
    And user of browser selects "POSIX" permission type in active modal
    And user of browser clicks on input box in active modal
    And user of browser sets "775" permission code in active modal
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared  

    #Check status-change time
    And user1 waits 2 seconds
    Then status-change time of user1's space1/file1 is greater than modification time

    # Clean up
    And user1 deletes files [space1/file1]
    
    
  Scenario: User changes directory permission using web gui and using oneclient he sees that status-change time has changed

    # Create directory
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    
	# Change permission code
    And user of browser selects "dir1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Change element permissions"
    And user of browser sees that "Edit permissions" modal has appeared
    And user of browser selects "POSIX" permission type in active modal
    And user of browser clicks on input box in active modal
    And user of browser sets "664" permission code in active modal
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared  
    
    #Check status-change time
    And user1 waits 2 seconds
    Then status-change time of user1's space1/dir1 is greater than modification time

    # Clean up
    And user1 deletes empty directories [space1/dir1]


  Scenario: User creates file using oneclient and changes its permission using web gui

    # Create file       
    When user1 creates regular files [space1/file1]

	# Change permission code
    And user of browser uses spaces select to change data space to "space1"
    And user of browser refreshes site
    And user of browser selects "file1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Change element permissions"
    And user of browser sees that "Edit permissions" modal has appeared
    And user of browser selects "POSIX" permission type in active modal
    And user of browser clicks on input box in active modal
    And user of browser sets "775" permission code in active modal
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared  

    #Check permission code
    Then user1 waits 2 seconds
    And mode of user1's space1/file1 is 775

    # Clean up
    And user1 deletes files [space1/file1]
        
        
  Scenario: User creates directory using oneclient and changes its permission using web gui

    # Create directory
    When user1 creates directories [space1/dir1]
    
    # Change permission code
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser selects "dir1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Change element permissions"
    And user of browser sees that "Edit permissions" modal has appeared
    And user of browser selects "POSIX" permission type in active modal
    And user of browser clicks on input box in active modal
    And user of browser sets "664" permission code in active modal
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared  

    # Check permission code
    Then user1 waits 2 seconds
    Then mode of user1's space1/dir1 is 664

    # Clean up
    And user1 deletes empty directories [space1/dir1]


  Scenario: User creates file using web gui and changes its permission using oneclient
    
    # Create file
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared

    # Change permission code
    And user1 changes space1/file1 mode to 775
    
    # Check permission code
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser selects "file1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Change element permissions"
    And user of browser sees that "Edit permissions" modal has appeared
    And user of browser selects "POSIX" permission type in active modal
    Then user of browser sees that current permission is "775"

    # Clean up
    And user1 deletes files [space1/file1]


  Scenario: User creates directory using web gui and changes its permission using oneclient
    
    # Create directory
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared

    # Change permission code
    And user1 changes space1/dir1 mode to 664
    
    # Check permission code
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser selects "dir1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Change element permissions"
    And user of browser sees that "Edit permissions" modal has appeared
    And user of browser selects "POSIX" permission type in active modal
    Then user of browser sees that current permission is "664"

    # Clean up
    And user1 deletes empty directories [space1/dir1]

