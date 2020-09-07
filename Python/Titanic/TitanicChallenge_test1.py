from sklearn.ensemble import RandomForestClassifier
import pandas as pd
import os


 


def loadFiles():
    for dirname, _, filenames in os.walk('input/'):
        for filename in filenames:
            print(os.path.join(dirname, filename))
    train_data = pd.read_csv("input/train.csv")
    # train_data.head()
    test_data = pd.read_csv("input/test.csv")
    # test_data.head()
    return train_data, test_data


def trys(train_data, test_data):
    women = train_data.loc[train_data.Sex == 'female']["Survived"]
    rate_women = sum(women)/len(women)    
    print("% of women who survived:", rate_women)
    
    men = train_data.loc[train_data.Sex == 'male']["Survived"]
    rate_men = sum(men)/len(men)
    print("% of men who survived:", rate_men)
    
def randomForestClassifierModel(train_data, test_data):
    y = train_data["Survived"]
    
    features = ["Pclass", "Sex", "SibSp", "Parch"]
    X = pd.get_dummies(train_data[features])
    X_test = pd.get_dummies(test_data[features])
    
    model = RandomForestClassifier(n_estimators=100, max_depth=5, random_state=1)
    model.fit(X, y)
    predictions = model.predict(X_test)
    
    output = pd.DataFrame({'PassengerId': test_data.PassengerId, 'Survived': predictions})
    output.to_csv('my_submission.csv', index=False)
    print("Submission was successfully saved!")



if __name__ == "__main__":
    train_data, test_data = loadFiles()
    #trys(train_data, test_data)
    randomForestClassifierModel(train_data, test_data)