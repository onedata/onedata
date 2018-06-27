Feature: Oneprovider transfers functionality

  Background:
    Given user opened browser window
    And user of browser opened Onezone URL 
    And user of browser clicked on the "plgrid" login button
    And user of browser clicked on the "user1" link
    And user of browser expanded the "go to your files" Onezone sidebar panel
    And user of browser clicked on the "p1" provider in Onezone providers sidebar panel
    And user of browser clicked on the "Go to your files" button in provider popup
    And user of browser seen that Oneprovider session has started


  Scenario: User replicates file to remote provider
    When user of browser changes current space in data tab to "space4"
    And user of browser uploads file "large_file.txt"
    # Wait to ensure synchronization between providers
    And user of browser is idle for 2 seconds
    And user of browser sees file chunks for file "large_file.txt" as follows:
            p1: entirely filled
            p2: never synchronized

    And user of browser replicates "large_file.txt" to provider "p2"
    
    # Check that transfer appeared in transfer tab
    And user of browser clicks on the "transfers" tab in main menu sidebar
    And user of browser selects "space4" space in transfers tab

    Then user of browser waits for all transfers to start
    And user of browser waits for all transfers to finish
    And user of browser sees file in ended transfers:
            name: large_file.txt
            destination: p2
            username: user1
            total files: 1
            transferred: 45 MiB
            type: replication
            status: completed

    # Check transfer chart
    And user of browser expands first transfer record
    And user of browser sees that there is non-zero throughput in transfer chart

    And user of browser clicks on the "data" tab in main menu sidebar
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees file chunks for file "large_file.txt" as follows:
            p1: entirely filled
            p2: entirely filled

    # TODO remove after integrating with swagger
    And user of browser removes "large_file.txt" in file browser


  Scenario: User replicates directory to remote provider
    When user of browser changes current space in data tab to "space4"
    And user of browser creates directory "dir1"
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser uploads file "large_file.txt"
    And user of browser sees file chunks for file "large_file.txt" as follows:
            p1: entirely filled
            p2: never synchronized
    And user of browser changes current working directory to space4 using breadcrumbs
    # Wait to ensure synchronization between providers
    And user of browser is idle for 2 seconds
    And user of browser replicates "dir1" to provider "p2"
    
    # Check that transfer appeared in transfer tab
    And user of browser clicks on the "transfers" tab in main menu sidebar
    And user of browser selects "space4" space in transfers tab

    Then user of browser waits for all transfers to start
    And user of browser waits for all transfers to finish
    And user of browser sees directory in ended transfers:
            name: dir1
            destination: p2
            username: user1
            total files: 1
            transferred: 45 MiB
            type: replication
            status: completed

    # Check transfer chart
    And user of browser expands first transfer record
    And user of browser sees that there is non-zero throughput in transfer chart

    And user of browser clicks on the "data" tab in main menu sidebar
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser sees file chunks for file "large_file.txt" as follows:
            p1: entirely filled
            p2: entirely filled

    # TODO remove after integrating with swagger
    And user of browser changes current working directory to space4 using breadcrumbs
    And user of browser removes "dir1" in file browser


  Scenario: User tries to migrate file to too small space on remote provider
    When user of browser changes current space in data tab to "space2"
    And user of browser uploads file "large_file.txt"
    # Wait to ensure synchronization between providers
    And user of browser is idle for 2 seconds
    And user of browser migrates "large_file.txt" from provider "p1" to provider "p2"

    # Check that transfer appeared in transfer tab
    And user of browser clicks on the "transfers" tab in main menu sidebar
    And user of browser selects "space2" space in transfers tab

    Then user of browser waits for all transfers to start
    And user of browser waits for all transfers to finish
    And user of browser sees file in ended transfers:
            name: large_file.txt
            destination: p2
            username: user1
            total files: 1
            transferred: 0 B
            type: migration
            status: failed

    And user of browser clicks on the "data" tab in main menu sidebar
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees file chunks for file "large_file.txt" as follows:
            p1: entirely filled
            p2: entirely empty

    # TODO remove after integrating with swagger
    And user of browser removes "large_file.txt" in file browser


  Scenario: User tries to migrate directory to too small space on remote provider
    When user of browser changes current space in data tab to "space2"
    And user of browser creates directory "dir1"
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser uploads file "large_file.txt"
    And user of browser changes current working directory to space2 using breadcrumbs
    # Wait to ensure synchronization between providers
    And user of browser is idle for 2 seconds
    And user of browser migrates "dir1" from provider "p1" to provider "p2"

    # Check that transfer appeared in transfer tab
    And user of browser clicks on the "transfers" tab in main menu sidebar
    And user of browser selects "space2" space in transfers tab

    Then user of browser waits for all transfers to start
    And user of browser waits for all transfers to finish
    And user of browser sees directory in ended transfers:
            name: dir1
            destination: p2
            username: user1
            total files: 1
            transferred: 0 B
            type: migration
            status: failed
    
    And user of browser clicks on the "data" tab in main menu sidebar
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser sees file chunks for file "large_file.txt" as follows:
            p1: entirely filled
            p2: entirely empty

    # TODO remove after integrating with swagger
    And user of browser changes current working directory to space2 using breadcrumbs
    And user of browser removes "dir1" in file browser


  Scenario: User tries to replicate file to too small space on remote provider
    When user of browser changes current space in data tab to "space2"
    And user of browser uploads file "large_file.txt"
    # Wait to ensure synchronization between providers
    And user of browser is idle for 2 seconds
    And user of browser replicates "large_file.txt" to provider "p2"

    # Check that transfer appeared in transfer tab
    And user of browser clicks on the "transfers" tab in main menu sidebar
    And user of browser selects "space2" space in transfers tab

    Then user of browser waits for all transfers to start
    And user of browser waits for all transfers to finish
    And user of browser sees file in ended transfers:
            name: large_file.txt
            destination: p2
            username: user1
            total files: 1
            transferred: 0 B
            type: replication
            status: failed

    And user of browser clicks on the "data" tab in main menu sidebar
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees file chunks for file "large_file.txt" as follows:
            p1: entirely filled
            p2: entirely empty

    # TODO remove after integrating with swagger
    And user of browser removes "large_file.txt" in file browser
  
    
  Scenario: User tries to replicate directory to too small space on remote provider
    When user of browser changes current space in data tab to "space2"
    And user of browser creates directory "dir1"
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser uploads file "large_file.txt"
    And user of browser changes current working directory to space2 using breadcrumbs
    # Wait to ensure synchronization between providers
    And user of browser is idle for 2 seconds
    And user of browser replicates "dir1" to provider "p2"

    # Check that transfer appeared in transfer tab
    And user of browser clicks on the "transfers" tab in main menu sidebar
    And user of browser selects "space2" space in transfers tab

    Then user of browser waits for all transfers to start
    And user of browser waits for all transfers to finish
    And user of browser sees directory in ended transfers:
            name: dir1
            destination: p2
            username: user1
            total files: 1
            transferred: 0 B
            type: replication
            status: failed
    
    And user of browser clicks on the "data" tab in main menu sidebar
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser sees file chunks for file "large_file.txt" as follows:
            p1: entirely filled
            p2: entirely empty

    # TODO remove after integrating with swagger
    And user of browser changes current working directory to space2 using breadcrumbs
    And user of browser removes "dir1" in file browser
    

  Scenario: User replicates directory with file on current provider to the same provider
    When user of browser changes current space in data tab to "space4"
    And user of browser creates directory "dir1"
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser uploads file "large_file.txt"
    And user of browser changes current working directory to space4 using breadcrumbs
    # Wait to ensure synchronization between providers
    And user of browser is idle for 2 seconds
    And user of browser replicates "dir1" to provider "p1"

    # Check that transfer appeared in transfer tab
    And user of browser clicks on the "transfers" tab in main menu sidebar
    And user of browser selects "space4" space in transfers tab

    Then user of browser waits for all transfers to start
    And user of browser waits for all transfers to finish
    And user of browser sees directory in ended transfers:
            name: dir1
            destination: p1
            username: user1
            total files: 0
            transferred: 0 B
            type: replication
            status: completed
    
    And user of browser clicks on the "data" tab in main menu sidebar
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser sees file chunks for file "large_file.txt" as follows:
            p1: entirely filled
            p2: never synchronized

    # TODO remove after integrating with swagger
    And user of browser changes current working directory to space4 using breadcrumbs
    And user of browser removes "dir1" in file browser


  Scenario: User migrates file to remote provider
    When user of browser changes current space in data tab to "space4"
    And user of browser uploads file "large_file.txt"
    # Wait to ensure synchronization between providers
    And user of browser is idle for 2 seconds
    And user of browser sees file chunks for file "large_file.txt" as follows:
            p1: entirely filled
            p2: never synchronized

    And user of browser migrates "large_file.txt" from provider "p1" to provider "p2"

    # Check that transfer appeared in transfer tab
    And user of browser clicks on the "transfers" tab in main menu sidebar
    And user of browser selects "space4" space in transfers tab

    Then user of browser waits for all transfers to start
    And user of browser waits for all transfers to finish
    And user of browser sees file in ended transfers:
            name: large_file.txt
            destination: p2
            username: user1
            total files: 2
            transferred: 45 MiB
            type: migration
            status: completed

    # Check transfer chart
    And user of browser expands first transfer record
    And user of browser sees that there is non-zero throughput in transfer chart

    And user of browser clicks on the "data" tab in main menu sidebar
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees file chunks for file "large_file.txt" as follows:
            p1: entirely empty
            p2: entirely filled

    # TODO remove after integrating with swagger
    And user of browser removes "large_file.txt" in file browser


  Scenario: User migrates directory to remote provider
    When user of browser changes current space in data tab to "space4"
    And user of browser creates directory "dir1"
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser uploads file "large_file.txt"
    And user of browser sees file chunks for file "large_file.txt" as follows:
            p1: entirely filled
            p2: never synchronized
    And user of browser changes current working directory to space4 using breadcrumbs
    # Wait to ensure synchronization between providers
    And user of browser is idle for 2 seconds
    And user of browser migrates "dir1" from provider "p1" to provider "p2"

    # Check that transfer appeared in transfer tab
    And user of browser clicks on the "transfers" tab in main menu sidebar
    And user of browser selects "space4" space in transfers tab

    Then user of browser waits for all transfers to start
    And user of browser waits for all transfers to finish
    And user of browser sees directory in ended transfers:
            name: dir1
            destination: p2
            username: user1
            total files: 2
            transferred: 45 MiB
            type: migration
            status: completed

    And user of browser clicks on the "data" tab in main menu sidebar
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser sees file chunks for file "large_file.txt" as follows:
            p1: entirely empty
            p2: entirely filled

    # TODO remove after integrating with swagger
    And user of browser changes current working directory to space4 using breadcrumbs
    And user of browser removes "dir1" in file browser
