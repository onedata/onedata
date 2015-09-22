Feature: Directory_stat

  Background:
    Given environment is defined in env.json
    Given environment is up
    Given u1 mounts onedata spaces in /root/onedata using token
    Given we are in space s1

  Scenario: Check file type
    When u1 creates directories [dir1]
    Then dir1 file type is directory
    Then last operation succeeds
    Then clean succeeds

  Scenario: Check default access permissions
    When u1 creates directories [dir1]
    Then dir1 mode is 755
    Then clean succeeds

  Scenario: Change access permissions
    When u1 creates directories [dir1]
    When u1 changes dir1 mode to 111
    Then last operation succeeds
    Then dir1 mode is 111
    Then clean succeeds

