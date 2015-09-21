Feature: Directory_stat

  Background:
    Given environment is defined in env.json
    Given environment is up
    Given u1 mounts onedata spaces in /root/onedata using token
    Given we are in space s1

  Scenario: Check file type
    When user creates directories [dir1]
    Then dir1 file type is directory

  Scenario: Check default access permissions
    When user creates directories [dir1]
    Then dir1 mode is 755

  Scenario Outline: Change access permissions
    When user creates directories [dir1]
    When user changes dir1 mode to <new_mod>
    Then last operation suceeds
    Then dir1 mode is <new_mod>

    Examples:
    | new_mod |
    | 754     |
    | 541     |
    | 777     |




