Feature: Oneprovider transfers functionality using multiple browser instances

  Background:
    Given user opened [browser1, browser2] window
    And user of [browser1, browser2] opened Onezone URL 
    And user of [browser1, browser2] clicked on the "plgrid" login button
    And user of [browser1, browser2] logged as [user1, user1]
    And user of [browser1, browser2] expanded the "go to your files" Onezone sidebar panel
    And user of [browser1, browser2] clicked on the [p1, p2] provider in Onezone providers sidebar panel
    And user of [browser1, browser2] clicked on the "Go to your files" button in provider popup
    And user of [browser1, browser2] seen that Oneprovider session has started

  Scenario: User replicates file from remote provider to current provider
    When user of browser1 changes current space in data tab to "space4"
    And user of browser1 uploads file "large_file.txt"
    # Wait to ensure synchronization between providers
    And user of browser1 is idle for 10 seconds
    And user of browser1 sees file chunks for file "large_file.txt" as follows:
            p1: entirely filled
            p2: never synchronized

    And user of browser2 changes current space in data tab to "space4"
    And user of browser2 replicates "large_file.txt" to provider "p2"

    # Check that transfer appeared in transfer tab
    And user of browser1 clicks on the "transfers" tab in main menu sidebar
    And user of browser1 selects "space4" space in transfers tab
    And user of browser1 waits for all transfers to start
    And user of browser1 waits for all transfers to finish
    Then user of browser1 sees file in ended transfers:
            name: large_file.txt
            destination: p2
            username: user1
            total files: 1
            transferred: 45 MiB
            type: replication
            status: completed

    # Check transfer chart
    And user of browser1 expands first transfer record
    And user of browser1 sees that there is non-zero throughput in transfer chart

    And user of browser2 clicks on the "data" tab in main menu sidebar
    And user of browser2 changes current space in data tab to "space4"
    And user of browser2 sees file browser in data tab in Oneprovider page
    And user of browser2 sees file chunks for file "large_file.txt" as follows:
            p1: entirely filled
            p2: entirely filled

    # TODO remove after integrating with swagger
    And user of browser2 removes "large_file.txt" in file browser


  Scenario: User replicates directory with 2 files on different providers to current provider
    When user of browser1 changes current space in data tab to "space4"
    And user of browser1 creates directory "dir1"
    And user of browser1 double clicks on item named "dir1" in file browser
    And user of browser1 uploads file "large_file.txt"
    And user of browser1 sees file chunks for file "large_file.txt" as follows:
            p1: entirely filled
            p2: never synchronized

    And user of browser2 changes current space in data tab to "space4"
    # Wait to ensure synchronization between providers
    And user of browser2 is idle for 2 seconds
    And user of browser2 refreshes site
    And user of browser2 sees file browser in data tab in Oneprovider page
    And user of browser2 double clicks on item named "dir1" in file browser
    And user of browser2 uploads file "large_file.txt"
    And user of browser2 is idle for 2 seconds
    And user of browser2 sees file chunks for file "large_file(1).txt" as follows:
            p1: never synchronized
            p2: entirely filled

    # Wait to ensure synchronization between providers
    And user of browser2 is idle for 2 seconds

    And user of browser2 changes current working directory to space4 using breadcrumbs
    And user of browser2 replicates "dir1" to provider "p2"
    
    # Check that transfer appeared in transfer tab
    And user of browser1 clicks on the "transfers" tab in main menu sidebar
    And user of browser1 selects "space4" space in transfers tab
    And user of browser1 waits for all transfers to start
    And user of browser1 waits for all transfers to finish
    Then user of browser1 sees directory in ended transfers:
            name: dir1
            destination: p2
            username: user1
            total files: 1
            transferred: 45 MiB
            type: replication
            status: completed

    # Check transfer chart
    And user of browser1 expands first transfer record
    And user of browser1 sees that there is non-zero throughput in transfer chart

    And user of browser1 clicks on the "data" tab in main menu sidebar
    And user of browser1 changes current space in data tab to "space4"
    And user of browser1 sees file browser in data tab in Oneprovider page
    And user of browser1 double clicks on item named "dir1" in file browser
    And user of browser1 refreshes site
    And user of browser1 sees file chunks for file "large_file.txt" as follows:
            p1: entirely filled
            p2: entirely filled
    And user of browser1 sees file chunks for file "large_file(1).txt" as follows:
            p1: never synchronized
            p2: entirely filled

    # TODO remove after integrating with swagger
    And user of browser1 changes current working directory to space4 using breadcrumbs
    And user of browser1 removes "dir1" in file browser


  Scenario: User migrates file from remote provider to current provider
    When user of browser1 changes current space in data tab to "space4"
    And user of browser1 uploads file "large_file.txt"
    # Wait to ensure synchronization between providers
    And user of browser1 is idle for 10 seconds
    And user of browser1 sees file chunks for file "large_file.txt" as follows:
            p1: entirely filled
            p2: never synchronized

    And user of browser2 changes current space in data tab to "space4"
    And user of browser2 migrates "large_file.txt" from provider "p1" to provider "p2"

    # Check that transfer appeared in transfer tab
    And user of browser1 clicks on the "transfers" tab in main menu sidebar
    And user of browser1 selects "space4" space in transfers tab
    And user of browser1 waits for all transfers to start
    And user of browser1 waits for all transfers to finish
    Then user of browser1 sees file in ended transfers:
            name: large_file.txt
            destination: p2
            username: user1
            total files: 2
            transferred: 45 MiB
            type: migration
            status: completed

    # Check transfer chart
    And user of browser1 expands first transfer record
    And user of browser1 sees that there is non-zero throughput in transfer chart

    And user of browser2 clicks on the "data" tab in main menu sidebar
    And user of browser2 changes current space in data tab to "space4"
    And user of browser2 sees file browser in data tab in Oneprovider page
    And user of browser2 sees file chunks for file "large_file.txt" as follows:
            p1: entirely empty
            p2: entirely filled

    # TODO remove after integrating with swagger
    And user of browser2 removes "large_file.txt" in file browser


  Scenario: User migrates directory with 2 files on different providers to current provider
    When user of browser1 changes current space in data tab to "space4"
    And user of browser1 creates directory "dir1"
    And user of browser1 double clicks on item named "dir1" in file browser
    And user of browser1 uploads file "large_file.txt"
    And user of browser1 sees file chunks for file "large_file.txt" as follows:
            p1: entirely filled
            p2: never synchronized

    And user of browser2 changes current space in data tab to "space4"
    # Wait to ensure synchronization between providers
    And user of browser2 is idle for 10 seconds
    And user of browser2 refreshes site
    And user of browser2 sees file browser in data tab in Oneprovider page
    And user of browser2 double clicks on item named "dir1" in file browser
    And user of browser2 uploads file "large_file.txt"
    And user of browser2 is idle for 2 seconds
    And user of browser2 sees file chunks for file "large_file(1).txt" as follows:
            p1: never synchronized
            p2: entirely filled

    # Wait to ensure synchronization between providers
    And user of browser2 is idle for 2 seconds

    And user of browser2 changes current working directory to space4 using breadcrumbs
    And user of browser2 migrates "dir1" from provider "p1" to provider "p2"
    
    # Check that transfer appeared in transfer tab
    And user of browser1 clicks on the "transfers" tab in main menu sidebar
    And user of browser1 selects "space4" space in transfers tab
    And user of browser1 waits for all transfers to start
    And user of browser1 waits for all transfers to finish
    Then user of browser1 sees directory in ended transfers:
            name: dir1
            destination: p2
            username: user1
            total files: 2
            transferred: 45 MiB
            type: migration
            status: completed

    # Check transfer chart
    And user of browser1 expands first transfer record
    And user of browser1 sees that there is non-zero throughput in transfer chart

    And user of browser1 clicks on the "data" tab in main menu sidebar
    And user of browser1 changes current space in data tab to "space4"
    And user of browser1 sees file browser in data tab in Oneprovider page
    And user of browser1 double clicks on item named "dir1" in file browser
    And user of browser1 refreshes site
    And user of browser1 sees file chunks for file "large_file.txt" as follows:
            p1: never synchronized
            p2: entirely filled
    And user of browser1 sees file chunks for file "large_file(1).txt" as follows:
            p1: entirely empty
            p2: entirely filled

    # TODO remove after integrating with swagger
    And user of browser1 changes current working directory to space4 using breadcrumbs
    And user of browser1 removes "dir1" in file browser
