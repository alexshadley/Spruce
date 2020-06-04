type Weekday {
    Monday
    Tuesday
    Wednesday
    Thursday
    Friday
    Saturday
    Sunday
}

tomorrow(day: Weekday) -> Weekday {
    case day {
        Monday -> Tuesday
        Tuesday -> Wednesday
        Wednesday -> Thursday
        Thursday -> Friday
        Friday -> Saturday
        Saturday -> Sunday
        Sunday -> Monday
    }
}

isWeekend(day: Weekday) -> Bool {
    case day {
        Monday -> False
        Tuesday -> False
        Wednesday -> False
        Thursday -> False
        Friday -> False
        Saturday -> True
        Sunday -> True
    }
}
