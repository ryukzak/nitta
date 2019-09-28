import BaseApiService, { RequestContentType } from "./base/BaseApiService";
import Todo from "./entities/Todo";

export class TodoApiService extends BaseApiService {
  constructor() {
    super("https://todo-backend-golang-goa.herokuapp.com/", RequestContentType.JSON);
  }

  async getTodos() {
    return this.doGet("todos") as Promise<Todo[]>;
  }

  async postTodo(title: string) {
    return this.doPost("todos", {}, { title });
  }

  async updateTodo(todo: Todo) {
    return this.doPatch(`todos/${todo.id}`, undefined, todo);
  }

  async deleteTodo(id: number) {
    return this.doDelete(`todos/${id}`);
  }
}

let todo_backend = new TodoApiService();
export default todo_backend;
